#FLP 2nd Project
#Jordan Jarolim, xjarol03, FIT VUTBR
#1. 5. 2017
#Makefile

make:
	swipl -o flp17-log -q -g main -t true -c flp17-log.pl
clean:
	rm -f flp17-log


