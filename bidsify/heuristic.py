import os


def create_key(template, outtype=('nii.gz',), annotation_classes=None):
    if template is None or not template:
        raise ValueError('Template must be a valid format string')
    return template, outtype, annotation_classes


def infotodict(seqinfo):
    """Heuristic evaluator for determining which runs belong where

    allowed template fields - follow python string module:

    item: index within category
    subject: participant id
    seqitem: run number during scanning
    subindex: sub index within group
    """

    # paths done in BIDS format
    t1w = create_key('sub-{subject}/anat/sub-{subject}_T1w')
    t2w = create_key('sub-{subject}/anat/sub-{subject}_T2w')
    func
    sbref
    fieldmap
    info = {t1w: [], func: []}

    data = create_key('run{item:03d}')
    info = {data: []}
    last_run = len(seqinfo)

    for s in seqinfo:

        info[data].append(s.series_id)
    return info
