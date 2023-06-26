#!/usr/bin/env bash
sudo docker run -it --rm \
    -v /home/azureuser/datadrive/pew-connectivity:/home/omniscape \
    -w /home/omniscape \
    -e USER=omniscape \
    -e JULIA_NUM_THREADS=$4 \
	circuitscape:latest