## @author     Dmitry Kolesnikov, <dmkolesnikov@gmail.com>
## @copyright  (c) 2012 - 2014 Dmitry Kolesnikov. All Rights Reserved
##
## @description
##   Makefile to build and release Erlang applications using
##   rebar, reltool, etc (see README for details)
##
## @version 0.5.0
.PHONY: test rel deps all pkg

#####################################################################
##
## application config
##
#####################################################################
ROOT  = `pwd`
PREFIX ?= /usr/local
APP ?= $(notdir $(CURDIR))
ARCH?= $(shell uname -m)
PLAT?= $(shell uname -s)
TAG  = ${ARCH}.${PLAT}
TEST?= priv/${APP}.benchmark
S3   =
GIT ?= 
VMI  = 
NET ?= lo0

## root path to benchmark framework
BB     = ../basho_bench
SSHENV = /tmp/ssh-agent.conf
ADDR  ?= $(shell ifconfig ${NET} | sed -En 's/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')

## erlang flags (make run only)
EFLAGS = \
	-name ${APP}@${ADDR} \
	-setcookie nocookie \
	-pa ./ebin \
	-pa deps/*/ebin \
	-pa apps/*/ebin \
	-kernel inet_dist_listen_min 32100 \
	-kernel inet_dist_listen_max 32199 \
	+P 1000000 \
	+K true +A 160 -sbt ts

#####################################################################
##
## internal config
##
#####################################################################
ifeq ($(wildcard rel/reltool.config),) 
	REL =
	VSN =
	TAR =
	PKG =
else
	REL  = $(shell cat rel/reltool.config | sed -n 's/{target_dir,.*\"\(.*\)\"}./\1/p')
	VSN  = $(shell echo ${REL} | sed -n 's/.*-\(.*\)/\1/p')
ifeq (${VSN},)
	VSN  = $(shell cat rel/reltool.config | sed -n 's/.*{rel,.*\".*\",.*\"\(.*\)\".*/\1/p')
endif
ifeq (${config},)
	RFLAGS  =	
	VARIANT =
else
	VARIANT = $(addprefix ., $(notdir $(basename ${config})))
	RFLAGS  = target_dir=${REL}${VARIANT} overlay_vars=${ROOT}/${config}
endif
ifeq (${VSN},)
	TAR = ${REL}${VARIANT}.${TAG}.tgz
	PKG = ${REL}${VARIANT}.${TAG}.bundle
else
	TAR = ${REL}-${VSN}${VARIANT}.${TAG}.tgz
	PKG = ${REL}-${VSN}${VARIANT}.${TAG}.bundle
endif
endif

## self-extracting bundle wrapper
BUNDLE_INIT = PREFIX=${PREFIX}\nREL=${PREFIX}/${REL}${VARIANT}\nAPP=${APP}\nVSN=${VSN}\nLINE=\`grep -a -n 'BUNDLE:\x24' \x240\`\ntail -n +\x24(( \x24{LINE\x25\x25:*} + 1)) \x240 | gzip -vdc - | tar -C ${PREFIX} -xvf - > /dev/null\n
BUNDLE_FREE = exit\nBUNDLE:\n
BUILDER = "cd /tmp && git clone ${GIT}/${APP} && cd /tmp/${APP} && make && make pkg && sleep 300"

#####################################################################
##
## build
##
#####################################################################
all: rebar deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean ; \
	rm -rf test.*-temp-data ; \
	rm -rf tests ; \
	rm -rf log ; \
	rm -f  *.${TAG}.tgz ; \
	rm -f  *.${TAG}.bundle


distclean: clean 
	@./rebar delete-deps

test: all
	@./rebar skip_deps=true eunit

docs:
	@./rebar skip_deps=true doc

#####################################################################
##
## release
##
#####################################################################
ifneq (${REL},)

${PKG}: pkg

${TAR}: rel

## assemble VM release
rel: 
	@./rebar generate ${RFLAGS}; \
	cd rel ; tar -zcf ../${TAR} ${REL}${VARIANT}/; cd -

## package VM release to executable bundle
ifeq (${PLAT},$(shell uname -s))
pkg: rel/deploy.sh ${TAR}
	@printf "${BUNDLE_INIT}"  > ${PKG} ; \
	cat  rel/deploy.sh       >> ${PKG} ; \
	printf  "${BUNDLE_FREE}" >> ${PKG} ; \
	cat  ${TAR}              >> ${PKG} ; \
	chmod ugo+x  ${PKG}                ; \
	echo "==> bundle: ${PKG}"
else
ifneq (${VMI},)
pkg:
	@echo "==> docker run ${VMI}" ;\
	I=`docker run -d ${VMI} /bin/sh -c ${BUILDER}` ;\
	(docker attach $$I &) ;\
	docker cp $$I:/tmp/${APP}/${PKG} . 1> /dev/null 2>&1 ;\
	while [ $$? -ne 0 ] ;\
	do \
   	sleep 10 ;\
   	docker cp $$I:/tmp/${APP}/${PKG} . 1> /dev/null 2>&1 ;\
	done ;\
	docker kill $$I ;\
	docker rm $$I

endif
endif

## copy 'package' to s3
s3: ${PKG}
	aws s3 cp ${PKG} ${S3}/${APP}-latest${VARIANT}.${TAG}.bundle
endif

#####################################################################
##
## deploy
##
#####################################################################
ifneq (${host},)
${SSHENV}:
	@echo "==> ssh: config keys" ;\
	ssh-agent -s > ${SSHENV}

node: ${SSHENV}
	@echo "==> deploy: ${host}" ;\
	. ${SSHENV} ;\
	k=`basename ${pass}` ;\
	l=`ssh-add -l | grep $$k` ;\
	if [ -z "$$l" ] ; then \
		ssh-add ${pass} ;\
	fi ;\
	rsync -cav --rsh=ssh --progress ${PKG} ${host}:${PKG} ;\
	ssh -t ${host} "sudo sh ./${PKG}"

endif

#####################################################################
##
## debug
##
#####################################################################
run:
	@erl ${EFLAGS}

benchmark:
	@echo "==> benchmark: ${TEST}" ;\
	$(BB)/basho_bench -N bb@127.0.0.1 -C nocookie ${TEST} ;\
	$(BB)/priv/summary.r -i tests/current ;\
	open tests/current/summary.png

ifneq (${REL},)
start: 
	@./rel/${REL}${VARIANT}/bin/${APP} start

stop:
	@./rel/${REL}${VARIANT}/bin/${APP} stop

console: 
	@./rel/${REL}${VARIANT}/bin/${APP} console

attach:
	@./rel/${REL}${VARIANT}/bin/${APP} attach
endif

#####################################################################
##
## dependencies
##
#####################################################################
rebar:
	@curl -L -O https://raw.githubusercontent.com/wiki/basho/rebar/rebar ; \
	chmod ugo+x rebar
