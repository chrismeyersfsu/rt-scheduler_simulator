# $Id: Makefile,v 1.1 2008/11/23 02:02:56 baker Exp baker $
.PHONY : clean all archive plots save data
PATH := /usr/local/ada-4.3/bin:${PATH}
PROGRAMS=test_one test_two test_three test_zero test_four test_five test_six test_tbs test_bgs test_bgs_ap test_tbs_ap
SOURCES=\
aperiodic_server_parameters.ads \
aperiodic_workloads.adb \
aperiodic_workloads.ads \
cycles.ads \
cycles.adb \
error_log.adb \
error_log.ads \
events.adb \
events.ads \
generic_queues.adb \
generic_queues.ads \
job_queues.ads \
jobs.adb \
jobs.ads \
periodic_workloads.adb \
periodic_workloads.ads \
random_tools.adb \
random_tools.ads \
replenishments.adb \
replenishments.ads \
report.adb \
report.ads \
simulator.adb \
simulator.ads \
sizes.ads \
stats.adb \
stats.ads \
tasks-aperiodic_server_data.ads  \
tasks.adb \
tasks.ads \
test_zero.adb \
test_one.adb \
test_two.adb \
test_three.adb \
test_four.adb \
test_five.adb \
test_six.adb \
threads-aperiodic_policies.ads \
threads-sched_bgs.adb \
threads-sched_bgs.ads \
threads-sched_pls.adb \
threads-sched_pls.ads \
threads-sched_dss.adb \
threads-sched_dss.ads \
threads-sched_edf.adb \
threads-sched_edf.ads \
threads-sched_tbs.adb \
threads-sched_tbs.ads \
threads.adb \
threads.ads \
virtual_times.ads \
workload_models.ads

default : $(PROGRAMS)
$(PROGRAMS) : $(SOURCES)
	gnatmake -gnata -gnato -gnat05 $@
clean:
	rm -f $(PROGRAMS) *~ *# b~*.ad* *.o *.ali *.log
save:
	-mv EDF* plots
data: test_four
	./test_four
plots: save
	cd plots; make
archive: 
	cd ..; tar czvpf 2008.tgz 2008
