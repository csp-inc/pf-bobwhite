#!/usr/bin/env bash
sudo docker run -it --rm \
    -v $(pwd):/home/cs \
    -w /home/cs \
    -e USER=cs \
    -e JULIA_NUM_THREADS=$8 \
	circuitscape:latest
