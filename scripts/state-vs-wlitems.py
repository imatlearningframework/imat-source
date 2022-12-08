#!/usr/bin/env python3

# Make a chart using ./mkplt.py <csv1> ...

import sys
from matplotlib import pyplot as plt
from matplotlib import patches as mpatches

def transpose(A):
    return list(map(list, zip(*A)))

def make_plot(filename, title, xlbl, ylbl, series):

    fig,ax1 = plt.subplots()
    ax1.set_yscale('log')
    ax1.set(xlabel=xlbl, ylabel=ylbl, title=title)

    legend_labels = []

    for s in series:
        (fn,data) = s
        xs = data[1] # DFA size column
        ys = data[3] # Worklist items column

        smallest = int(min(data[1]))
        cutoff = 4 # Smallest interesting DFA size for graphics
        largest = int(max(data[1]))
        distr = [()] * (int(max(data[1])) + 1)
        for i,x in enumerate(xs):
            distr[int(x)] += (ys[i],)
        pos = list(range(cutoff, largest+1))
        distr = distr[cutoff:]
        vp = ax1.violinplot(distr, pos, showmeans=True)
        legend_labels += [mpatches.Patch(color=vp['bodies'][0].get_facecolor().flatten())]

    ax1.legend(legend_labels, ['Baseline', 'Using unsat-cores'], loc='upper left')
    fig.tight_layout()
    fig.savefig(filename)

def main(csvs):
    series = []
    for csv in csvs:
        f = open(csv, 'r')
        lbls = list(map(lambda s: s.strip().strip('"'), f.readline().split(',')))

        data = []
        for r in f.readlines():
            spl = r.split(',')
            data += [[spl[0].strip('"')] + list(map(float, spl[1:]))]
        data = transpose(data)
        series += [(csv, data)]

    make_plot('plot.png', 'DFA size vs. Worklist items processed', 'Minimum DFA size',
                'Worklist items processed', series)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("usage: state-vs-wlitems.py <baseline-csv> <unsat-cores-csv>")
        sys.exit(1)
    main(sys.argv[1:])
