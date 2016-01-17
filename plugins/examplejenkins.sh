#!/bin/bash
BASE_URL=$1
JOBNAME=$2

# Need to provide a pem to talk to secure jenkins
CERT_PATH="path/to/cert.pem"

CMD="curl --silent -E ${CERT_PATH} -k https://${BASE_URL}/job/${JOBNAME}/lastBuild/api/json?pretty=true | grep \"result\" | awk '{print $3}'"

RESULT=$(eval $CMD)

if [[ $RESULT == *"SUCCESS"* ]]
then
		echo '1'     # This maps to the "Passing" format
elif [[ $RESULT == *"null"* ]]
then
		echo '3'     # This maps to the "Building" format
else
		echo '0'     # This maps to the "Failing" format
fi
