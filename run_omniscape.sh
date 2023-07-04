#!/usr/bin/env bash
sudo docker run -it --rm \
    -v $(pwd):/home/cs \
    -w /home/cs \
	-e JULIA_NUM_THREADS=14 \
	vlandau/omniscape:0.5.3
