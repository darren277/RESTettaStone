#!/usr/bin/bash

D_VERSION=$(wget -qO - http://downloads.dlang.org/releases/LATEST)
wget -O dmd.deb http://downloads.dlang.org/releases/2.x/${D_VERSION}/dmd_${D_VERSION}-0_amd64.deb
apt-get update
apt-get install -y ./dmd.deb
rm dmd.deb
