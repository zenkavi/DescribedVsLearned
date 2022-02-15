import csv

def load_trial_conditions_from_csv(trialsFileName):
    """
    Loads trial conditions from a CSV file. Format expected for trial
    conditions file: value_left, value_right.
    Args:
      trialsFileName: string, name of trial conditions file. 

    Returns:
      A list containing the trial conditions, where each trial condition is a
          tuple with format (value_left, value_right).
    """
    trialConditions = []
    try:
        with open(trialsFileName, u"rt") as csvfile:
            reader = csv.DictReader(csvfile)
            if (u"QVLeft" not in reader.fieldnames or
                u"QVRight" not in reader.fieldnames or
                u"EVRight" not in reader.fieldnames or
                u"EVLeft" not in reader.fieldnames or
                u"probFractalDraw" not in reader.fieldnames):
                raise RuntimeError(u"Missing field in trial conditions file. "
                                   "Fields required: QVLeft, QVRight, EVLeft, EVRight, probFractalDraw")
            for row in reader:
                trialConditions.append(
                    (float(row[u"QVLeft"]), float(row[u"QVRight"]), float(row[u"EVLeft"]), float(row[u"EVRight"]), float(row[u"probFractalDraw"])))
    except:
        print(u"Error while reading trial conditions file " + trialsFileName)
        raise
    return trialConditions