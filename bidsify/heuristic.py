import os

def create_key(template, outtype=('nii.gz',), annotation_classes=None):
    if template is None or not template:
        raise ValueError('Template must be a valid format string')
    return template, outtype, annotation_classes

def infotodict(seqinfo):

    # paths done in BIDS format
    t1w = create_key('sub-{subject}/anat/sub-{subject}_T1w') #anat
    t2w = create_key('sub-{subject}/anat/sub-{subject}_T2w') #anat
    t2_clinical = create_key('sub-{subject}/anat/sub-{subject}_T2clinical') #anat
    task = create_key('sub-{subject}/func/sub-{subject}_task-bundles_run-{item:01d}_bold') #func
    sbref = create_key('sub-{subject}/func/sub-{subject}_task-bundles_run-{item:01d}_sbref') #func
    fmap_pos = create_key('sub-{subject}/fmap/sub-{subject}_pos') #fmap
    fmap_neg = create_key('sub-{subject}/fmap/sub-{subject}_neg') #fmap

    info = {t1w: [], t2w: [], task: [], sbref: [], fmap_pos: [], fmap_neg: []}

    for idx, s in enumerate(seqinfo): #each row of dicominfo.tsv

        if (s.TE == 2.56) and ('T1w' in s.protocol_name):
          info[t1w] = [s.series_id]

        if (s.TE == 206) and ('T2w' in s.protocol_name):
          info[t2w] = [s.series_id]

        if (s.TE == 90) and ('T2_Clinical' in s.protocol_name):
           info[t2_clinical] = [s.series_id]

        if (s.dim4 == 892) and ('BOLD_MB' in s.protocol_name):
          info[task] = [s.series_id]

        if (s.dim4 == 1) and ('BOLD_MB' in s.protocol_name):
           info[sbref] = [s.series_id]

        if (s.TE == 50) and ('Fieldmap_Pos' in s.protocol_name):
           info[fmap_pos] = [s.series_id]

        if (s.TE == 50) and ('Fieldmap_Neg' in s.protocol_name):
           info[fmap_neg] = [s.series_id]

    return info
