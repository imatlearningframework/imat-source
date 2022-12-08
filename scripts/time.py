#!/usr/bin/env python3

import sys
from matplotlib import pyplot as plt
from matplotlib import patches as mpatches

def transpose(A):
    return list(map(list, zip(*A)))

def buckets(xs, ys):
    maxsize = int(max(xs))
    cutoff = 4 # Smallest interesting DFA size
    distr = [()] * (maxsize + 1)
    for i,x in enumerate(xs):
        distr[int(x)] += (ys[i],)
    pos = list(range(cutoff,maxsize + 1))
    return (distr[cutoff:], pos)

def make_plot(filename, title, xlbl, ylbl, data):

    fig,ax1 = plt.subplots()
    ax1.set_yscale('log')
    ax1.set(xlabel=xlbl, ylabel=ylbl, title=title)

    distr, pos = buckets(data[1], data[6])
    ax1.violinplot(distr, pos, showmeans=True)

    legend_labels = []
    fig.savefig(filename)

def main(csv):
    f = open(csv, 'r')
    lbls = list(map(lambda s: s.strip().strip('"'), f.readline().split(',')))

    data = []
    for r in f.readlines():
        spl = r.split(',')
        data += [[spl[0].strip('"')] + list(map(float, spl[1:]))]
    data = transpose(data)

    make_plot('plot.png', 'DFA size vs. Learning time', 'Minimum DFA size',
                'Total learning time', data)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("usage: time.py <csv>")
        sys.exit(1)
    main(sys.argv[1])
