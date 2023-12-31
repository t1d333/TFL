#!/bin/bash

if [[ $1 =~ ^a*(b|c)a*$ ]]; then
	echo 1
else
	echo 0
fi
	
