#!/bin/sh
export alifedir=`grep workingDir wain.config | sed 's/.*="//; s/"$//'`
export logdir=${alifedir}/log
export logname=`grep experimentName wain.config | sed 's/.*="//; s/"$//'`.log
export log=${logdir}/${logname}
export statsname=rawStats
export stats=${alifedir}/${statsname}
