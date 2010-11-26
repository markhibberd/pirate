MODULE = pirate
PROD = src/prod
TEST = src/test
DEMO = src/demo
GEN = gen
DOC_PROD = ${GEN}/doc/prod
PROD_CLS = gen/classes/prod
TEST_CLS = gen/classes/test
DEMO_CLS = gen/classes/demo
XRAY_PROD = ${PROD_CLS}.sxr
XRAY_DEMO = ${DEMO_CLS}.sxr
LIB = lib/run
SXR = lib/compile/sxr_2.8.0-0.2.7-SNAPSHOT.jar
CP = lib/run/\*:lib/test/\*
CP_PROD = ${CP}:${PROD_CLS}
CP_TEST = ${CP_PROD}:${TEST_CLS}
ETC = etc
DIST = gen/dist
JAR = ${DIST}/${MODULE}.jar
JAR_SRC = ${DIST}/${MODULE}-src.jar
PROGUARD = lib/build/proguard.jar
MIN_JAR = ${DIST}/${MODULE}.min.jar
TAR = ${DIST}/${MODULE}-${VERSION}.tar.gz
VERSION = 0.1
MANIFEST = ${ETC}/MANIFEST.MF
DIST_MANIFEST = ${GEN}/MANIFEST.MF
TAR_IMAGE = ${GEN}/image/${MODULE}-${VERSION}
RELEASE_DIR = ${GEN}/release/${VERSION}

.PHONY: clean

default: test dist

compile: clean ${PROD_CLS} ${TEST_CLS} ${DEMO_CLS}
	find ${PROD} -name "*.scala" | xargs -s 30000 fsc -Xplugin:${SXR} -P:sxr:base-directory:${PROD}  -classpath ${CP} -d ${PROD_CLS} && \
	find ${DEMO} -name "*.scala" | xargs -s 30000 fsc -Xplugin:${SXR} -P:sxr:base-directory:${DEMO}  -classpath ${CP_PROD} -d ${DEMO_CLS} && \
	find ${TEST} -name "*.scala" | xargs -s 30000 fsc -classpath ${CP_PROD} -d ${TEST_CLS} 

test: compile
	scala -cp ${CP_TEST} org.scalatest.tools.Runner -p ${TEST_CLS} -oDFW && \
	scala -cp ${CP_TEST} io.mth.pirate.CommandTest && \
	scala -cp ${CP_TEST} io.mth.pirate.ParserTest && \
	scala -cp ${CP_TEST} io.mth.pirate.FlagTest && \
	scala -cp ${CP_TEST} io.mth.pirate.TextTest && \
	scala -cp ${CP_TEST} io.mth.pirate.UsageTest

${JAR}: compile ${DIST_MANIFEST} ${DIST}
	jar cfm ${JAR} ${DIST_MANIFEST} -C ${PROD_CLS} .

${JAR_SRC}: ${DIST}
	jar cf ${JAR_SRC} -C ${PROD} .

${TAR}: doc ${JAR} ${JAR_SRC} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${TAR_IMAGE}/doc/xray ${DEMO_TARGET}
	cp -r ${DOC_PROD} ${TAR_IMAGE}/doc/scaladoc && \
	cp -r ${DEMO} ${TAR_IMAGE}/doc/ && \
	cp -r ${XRAY_PROD} ${TAR_IMAGE}/doc/xray/prod && \
	cp -r ${XRAY_DEMO} ${TAR_IMAGE}/doc/xray/demo && \
	cp lib/run/*.jar ${TAR_IMAGE}/lib && \
	cp ${JAR} ${JAR_SRC} ${TAR_IMAGE} && \
	cp LICENSE COPYING README ${TAR_IMAGE} && \
	cp -r ${ETC}/licenses ${TAR_IMAGE} && \
	tar cfz ${TAR} -C ${GEN}/image .

dist: clean ${TAR}

doc: ${DOC_PROD}
	(cd ${PROD} && \
	find io -name "*.scala" | xargs -s 30000 scaladoc -doc-title "scaladoc for [${MODULE}]" -doc-version ${VERSION} -doc-source-url http://pirate.mth.io/release/${VERSION}/doc/xray/prod -classpath ../../lib/run/\*:../../${PROD_CLS} -d ../../${DOC_PROD})

publish:
	rsync -aH --stats --exclude \*~ ${ETC}/www/ web@mth.io:pirate.mth.io/data

release: dist ${RELEASE_DIR}
	${ETC}/sha1 ${JAR} > ${RELEASE_DIR}/${MODULE}.jar.sha1 && \
	${ETC}/sha1 ${TAR} > ${RELEASE_DIR}/${MODULE}-${VERSION}.tar.gz.sha1 && \
	cp -r ${TAR_IMAGE}/doc ${RELEASE_DIR} && \
	rm -rf ${RELEASE_DIR}/doc/demo && \
	cp ${JAR} ${TAR} ${RELEASE_DIR} && \
	rsync -aH --stats --exclude \*~ ${RELEASE_DIR} web@mth.io:pirate.mth.io/data/release/.

${MIN_JAR}: ${JAR}
	java -jar ${PROGUARD} -injars lib/run/scala-library.jar:lib/run/scalaz-core_2.8.0-5.0.jar:${JAR} \
                              -outjar ${MIN_JAR} @etc/proguard.conf

${DIST_MANIFEST}: ${GEN}
	sed -e 's/VERSION/${VERSION}/' ${MANIFEST} > ${DIST_MANIFEST}

repl: compile
	scala -classpath ${CP}:${PROD_CLS}:${TEST_CLS}

size: 
	find ${PROD} -name "*.scala" | xargs wc | sort -n

simian:
	echo "implement me"

${GEN} ${GEN}/tmp ${DEMO_CLS} ${PROD_CLS} ${TEST_CLS} ${DIST} ${LIB} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${DOC_PROD} ${RELEASE_DIR} ${TAR_IMAGE}/doc/xray ${DEMO_TARGET}:
	mkdir -p $@

clean:
	rm -rf ${GEN}; find . -name "*~" -o -name "*.core" -print0 | xargs -0 rm
