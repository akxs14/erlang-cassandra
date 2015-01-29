#!/bin/sh
URL="http://localhost:9222/deviceinfo"
EXPECTED="{\\\"rest\\\": \\\"Hello\\\"} (HTTP 200)"
RESULT=`curl -sL -w " (HTTP %{http_code})\n" -i -H "Accept: application/json" $URL`
echo "$RESULT" | grep -q  "$EXPECTED"
RC=$?
if [ $RC -gt 0 ]
then
	echo "$RESULT";
fi
exit $RC 

