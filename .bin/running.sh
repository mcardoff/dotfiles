#!/bin/sh

stuff=$(ps -aux | grep -Fe $1)

if [ -z "$stuff" ]
then
    echo 'Hello world'
else
    echo 'Bye World'
fi
   
