
Initial command to explore DICOM structures and specify the `heuristics` file.

**IMPORTANT**: If you're using zsh, which is the new default on Mac Terminals you need to include `noglob` before running the docker image so it interprets the `*` wildcards correctly.

```
noglob docker run --rm -it -v /Users/zeynepenkavi/Downloads/GTavares_2017_arbitration:/base nipy/heudiconv:latest \
-d /base/raw_fMRI_data/AR-GT-BUNDLES-{subject}_RANGEL/*/*/*.IMA \
-o /base/Nifti/ \
-f convertall \
-s 01 \
-c none --overwrite
```
