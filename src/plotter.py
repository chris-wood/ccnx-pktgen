import sys
import numpy as np
import matplotlib.pyplot as plt

class Experiment(object):
    def __init__(self, name):
        self.name = name
        self.extract()
        self.process()

    def extract(self):
        data = self.name[0:self.name.index(".")].split("_")
        self.n = int(data[1])
        self.minsize = int(data[2])
        self.maxsize = int(data[3])
        self.minnl = int(data[4])
        self.maxnl = int(data[5])
        self.minnc = int(data[6])
        self.maxnc = int(data[7])
        self.windowSize = int(data[9])

    def process(self):
        points = []
        with open(self.name, "r") as fh:
            for line in fh:
                timeval = line.strip().split(",")[1]
                points.append(int(timeval))
        self.average = (reduce(lambda x, y: x + y, points) / len(points))

    def __str__(self):
        return "%d-[%d,%d]-[%d,%d]-[%d,%d]-%d" % (self.n, self.minsize, self.maxsize, self.minnl, self.maxnl, self.minnc, self.maxnc, self.windowSize)

fig = plt.figure()
ax = fig.add_subplot(111)

experiments = []
with open(sys.argv[1], "r") as fh:
    for line in fh:
        line = line.strip()
        experiments.append(Experiment(line))

N = len(experiments)

values = []
for e in experiments:
    values.append(e.average)

## necessary variables
ind = np.arange(N)                # the x locations for the groups
width = 0.5                      # the width of the bars

## the bars
rects1 = ax.bar(ind, values, width,
                color='black',
                error_kw=dict(elinewidth=2,ecolor='red'))

# axes and labels
ax.set_ylabel('Average Packet RTT')
ax.set_title('RTT')

print map(lambda e: str(e), experiments)
xTickMarks = map(lambda e: str(e), experiments)
ax.set_xticks(ind+width)
xtickNames = ax.set_xticklabels(xTickMarks)
plt.setp(xtickNames, rotation=45, fontsize=10)

plt.show()
