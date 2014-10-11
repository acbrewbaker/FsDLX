#!bin/bash
echo `fsharpc /u/css/ab67597/5483/Project1/assemble.fs -o /u/css/ab67597/5483/Project1/assemble.exe`
(cd /u/css/ab67597/5483/Project1/ && exec perl run.pl)