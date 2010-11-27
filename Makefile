MODULE = pirate
VERSION = 0.2

GEN = gen

SRC_PROD = src/prod
SRC_TEST = src/test
SRC_DEMO = src/demo

CLS_PROD = gen/classes/prod
CLS_TEST = gen/classes/test
CLS_DEMO = gen/classes/demo

CP_BASE = lib/run/\*:lib/test/\*
CP_PROD = ${CP_BASE}:${CLS_PROD}
CP_TEST = ${CP_PROD}:${CLS_TEST}

DOC_PROD = ${GEN}/doc/prod

XRAY = lib/compile/sxr_2.8.0-0.2.7-SNAPSHOT.jar
XRAY_PROD = ${CLS_PROD}.sxr
XRAY_DEMO = ${CLS_DEMO}.sxr

PROGUARD = lib/build/proguard.jar
PROGUARD_CONF = etc/proguard.conf

PUBLISH_WWW = web@mth.io:pirate.mth.io/data
PUBLISH_RELEASE = web@mth.io:pirate.mth.io/data/release/.

DIST = ${GEN}/dist

JAR = ${DIST}/${MODULE}.jar
JAR_SRC = ${DIST}/${MODULE}-src.jar
JAR_MIN = ${DIST}/${MODULE}.min.jar

TAR = ${DIST}/${MODULE}-${VERSION}.tar.gz

HASH = ${ETC}/sha1
HASH_JAR = ${JAR}.sha1
HASH_TAR = ${TAR}.sha1

LICENSES = etc/licenses
WWW = etc/www
MANIFEST = etc/MANIFEST.MF
DIST_MANIFEST = ${GEN}/MANIFEST.MF
TAR_IMAGE = ${GEN}/image/${MODULE}-${VERSION}
RELEASE_DIR = ${GEN}/release/${VERSION}

DIRECTORIES = ${GEN} ${GEN}/tmp ${CLS_DEMO} ${CLS_PROD} ${CLS_TEST} ${DIST} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${DOC_PROD} ${RELEASE_DIR} ${TAR_IMAGE}/doc/xray ${DEMO_TARGET}


.PHONY: clean dist doc compile www size repl publish release

default: test dist

compile: clean ${CLS_PROD} ${CLS_TEST} ${CLS_DEMO}
	find ${SRC_PROD} -name "*.scala" | xargs -s 30000 fsc -Xplugin:${XRAY} -P:sxr:base-directory:${SRC_PROD}  -classpath ${CP_BASE} -d ${CLS_PROD} && \
	find ${SRC_DEMO} -name "*.scala" | xargs -s 30000 fsc -Xplugin:${XRAY} -P:sxr:base-directory:${SRC_DEMO}  -classpath ${CP_PROD} -d ${CLS_DEMO} && \
	find ${SRC_TEST} -name "*.scala" | xargs -s 30000 fsc -classpath ${CP_PROD} -d ${CLS_TEST} 

test: compile
	scala -cp ${CP_TEST} org.scalatest.tools.Runner -p ${CLS_TEST} -oDFW && \
	scala -cp ${CP_TEST} io.mth.pirate.CommandTest && \
	scala -cp ${CP_TEST} io.mth.pirate.ParserTest && \
	scala -cp ${CP_TEST} io.mth.pirate.FlagTest && \
	scala -cp ${CP_TEST} io.mth.pirate.TextTest && \
	scala -cp ${CP_TEST} io.mth.pirate.UsageTest

${JAR}: compile ${DIST_MANIFEST} ${DIST}
	jar cfm ${JAR} ${DIST_MANIFEST} -C ${CLS_PROD} .

${JAR_SRC}: ${DIST}
	jar cf ${JAR_SRC} -C ${SRC_PROD} .

${JAR_MIN}: ${JAR}
	java -jar ${PROGUARD} \
		-injars lib/run/scala-library.jar:lib/run/scalaz-core_2.8.0-5.0.jar:${JAR} \
		-outjar ${JAR_MIN} @${PROGUARD_CONF}

${TAR}: doc ${JAR} ${JAR_SRC} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${TAR_IMAGE}/doc/xray ${DEMO_TARGET}
	cp -r ${DOC_PROD} ${TAR_IMAGE}/doc/api && \
	cp -r ${SRC_DEMO} ${TAR_IMAGE}/. && \
	cp -r ${XRAY_PROD} ${TAR_IMAGE}/doc/xray/prod && \
	cp -r ${XRAY_DEMO} ${TAR_IMAGE}/doc/xray/demo && \
	cp lib/run/*.jar ${TAR_IMAGE}/lib && \
	cp ${JAR} ${JAR_SRC} ${TAR_IMAGE} && \
	cp LICENSE COPYING README ${TAR_IMAGE} && \
	cp -r ${LICENSES} ${TAR_IMAGE} && \
	tar cfz ${TAR} -C ${GEN}/image .

dist: clean ${TAR}

doc: ${DOC_PROD}
	(cd ${SRC_PROD} && \
	find io -name "*.scala" | xargs -s 30000 \
		scaladoc \
			-doc-title "scaladoc for [${MODULE} ${VERSION}]" \
			-doc-version ${VERSION} \
			-doc-source-url http://pirate.mth.io/release/${VERSION}/doc/xray/prod \
			-classpath ../../lib/run/\*:../../${CLS_PROD} \
			-d ../../${DOC_PROD})

${HASH_JAR}:
	${HASH} ${JAR} > ${HASH_JAR}

${HASH_TAR}:
	${HASH} ${TAR} > ${HASH_TAR}

www:
	rsync -aH --stats --exclude \*~ ${WWW}/ ${PUBLISH_WWW}

release: dist ${RELEASE_DIR} ${HASH_JAR} ${HASH_TAR}
	cp -r ${TAR_IMAGE}/doc ${RELEASE_DIR} && \
	cp ${JAR} ${HASH_JAR} ${TAR} ${HASH_TAR} ${RELEASE_DIR} && \

publish: release
	rsync -aH --stats --exclude \*~ ${RELEASE_DIR} ${PUBLISH_RELEASE}

${DIST_MANIFEST}: ${GEN}
	sed -e 's/VERSION/${VERSION}/' ${MANIFEST} > ${DIST_MANIFEST}

repl: compile
	scala -classpath ${CP_BASE}:${CLS_PROD}:${CLS_TEST}

size: 
	find ${SRC_PROD} -name "*.scala" | xargs wc | sort -n

${DIRECTORIES}:
	mkdir -p $@

clean:
	rm -rf ${GEN}; find . -name "*~" -o -name "*.core" -print0 | xargs -0 rm
