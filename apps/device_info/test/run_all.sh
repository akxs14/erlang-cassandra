#!/bin/sh
RC=0
for i in *.sh; do
	if [ "`basename $i`" != "`basename $0`" ];
	then
		./$i;
		temp_RC=$?
		if [ $temp_RC -gt 0 ];
		then
			echo "$i: Test failed.";
			RC=$temp_RC;
		else	echo "$i: Test successful";
		fi
	fi
done

exit $RC
