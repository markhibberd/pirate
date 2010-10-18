MODULE = pirate
PROD = src/prod
TEST = src/test
CP = lib/prod/\*:lib/test/\*
CP_PROD = ${CP}:${PROD_CLS}
CP_TEST = ${CP_PROD}:${TEST_CLS}
GEN = gen
PROD_CLS = gen/prod/classes
TEST_CLS = gen/test/classes
DIST = gen/dist
JAR = ${DIST}/${MODULE}.jar

.PHONY: clean

default: test dist

compile: ${PROD_CLS} ${TEST_CLS}
	find ${PROD} -name "*.scala"  | xargs fsc -unchecked -classpath ${CP} -d ${PROD_CLS} && \
	find ${TEST} -name "*.scala"  | xargs fsc -unchecked -classpath ${CP_PROD} -d ${TEST_CLS}

test: compile
	scala -cp ${CP_TEST} org.scalatest.tools.Runner -p ${TEST_CLS} -o && \
        scala -cp ${CP_TEST} scala.io.mth.pirate.test.PirateTest

dist:  compile ${DIST}
	jar cf ${JAR} -C ${PROD_CLS} .

repl: compile
	scala -classpath ${CP}:${PROD_CLS}:${TEST_CLS}

size: 
	find ${PROD} -name "*.scala" | xargs wc | sort -n

${GEN} ${PROD_CLS} ${TEST_CLS} ${DIST}:
	mkdir -p $@

clean:
	rm -rf ${GEN}; find . -name "*~" -print0 | xargs -0 rm

