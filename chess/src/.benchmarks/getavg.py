import os
import re
import statistics as stats


def get_info_from_ghc_info(ghc_info_str):
    """ Fetch the relevant memory usage, elapsed time, and GC time for a GHC info string, returning as a dictionary. """
    # Return it as a dictionary - makes it easier
    # Format:
    #   <<ghc: 104577794416 bytes, 2920 GCs, 1809257145/13082145336 avg/max bytes residency (18 samples), 27498M in use, 0.004 INIT (0.066 elapsed), 71.468 MUT (58.753 elapsed), 686.069 GC (403.051 elapsed) :ghc>>#
    return {
        "memory": int(re.search(r"([0-9]+)M in use", ghc_info_str).group(1)),
        "elapsed_time": float(re.search(r"MUT \(([0-9]+\.[0-9]+?) elapsed\)", ghc_info_str).group(1)),
        "gc_time": float(re.search(r"GC \(([0-9]+\.[0-9]+?) elapsed\)", ghc_info_str).group(1)),
        "gc_count": int(re.search(r"([0-9]+) GCs", ghc_info_str).group(1))
    }


def get_ghc_info(file):
    """ Return iterator of all the "<<ghc: ... :ghc>>" data for a given file. """
    with open(file, "r") as file_handle:
        return (x.group() for x in re.finditer(
            r"<<ghc: (.*?) :ghc>>", file_handle.read()))


def get_file_info(file):
    """ Return all GHC info for that file, in a dictionary. """
    dict = {}
    for info in get_ghc_info(file):
        results = get_info_from_ghc_info(info)
        for key in results:
            if (key in dict):
                dict[key].append(results[key])
            else:
                dict[key] = [results[key]]

    # print(dict)
    if len(dict) < 4:
        dict = {
            "memory": None,
            "elapsed_time": None,
            "gc_time": None,
            "gc_count": None
        }

    # Transform lists into values
    run_count = -1
    for key in [k for k in dict]:
        if dict[key] is not None:
            dict[f"mean {key}"] = round(stats.mean(dict[key]), 2)
            dict[f"median {key}"] = round(stats.median(dict[key]), 2)
            dict[f"mode {key}"] = round(stats.mode(dict[key]), 2)

            # Sanity check for number of runs
            if run_count == -1:
                run_count = len(dict[key])
            elif run_count != len(dict[key]):
                raise Exception("Number of runs doesn't match")
        else:
            dict[f"mean {key}"] = None
            dict[f"median {key}"] = None
            dict[f"mode {key}"] = None
            if (run_count > 0):
                raise Exception("Number of runs doesn't match")
        del dict[key]
    dict["runs"] = (run_count if run_count > 0 else 0)
    return dict


def get_avgs_for_file(file):

    info = get_file_info(file)

    # Fetch info for that specific configuration
    pattern = r"(?:\.\/(.*?)\/)?([A-Za-z0-9]*)\.hs\.([0-9]{2}\.[0-9]{1,2})\.(?:(compactgc|newgc)(?:\.))?(?:(a[0-9]+[a-z])\.)?(g[0-9])(?:\.(newapproach)?)?"
    match = re.search(pattern, file.path)

    file_info = {
        "directory": match.group(1) + "/",
        "name": match.group(2),
        "resolver": match.group(3),
        "gc_type": match.group(4) or "oldgc",
        "memory_allocated": match.group(5) or "a1m",
        "generations": match.group(6),
        "newapproach": match.group(7) or "old"
    }
    for key in info:
        file_info[key] = info[key]

    return file_info


def getavg(path="."):
    all_info = []
    for file in os.scandir(path):
        if (file.is_file() and file.path.__contains__(".hs.")):
            file_info = get_avgs_for_file(file)
            all_info.append(file_info)
        elif (file.is_dir()):
            all_info += getavg(file.path)  # Recursive call

    return all_info


def print_info_to_csv_file(info_list):
    with open("averages.csv", "w") as file:
        keys = info_list[0].keys()
        lines = [",".join(keys)]  # .csv header
        lines += [",".join((str(info[key] if info[key] is not None else "") for key in keys))
                  for info in info_list]

        # Add newlines
        file.writelines((s + "\n" for s in lines))


print_info_to_csv_file(getavg())
