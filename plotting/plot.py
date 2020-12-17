import numpy as np
import matplotlib.pyplot as plt


def parseFile(fn):
	''' loads in a file and parses the data '''
	file = open(fn,'r')
	head = file.readline()
	head = head.strip().replace('<','').replace('>','').replace('#','').split()
	lines = file.readlines()

	dlines = np.array([line.strip().split() for line in lines if "#simulation restarted" not in line], dtype=np.float64)
	data = {k: dlines[:,i] for i,k in enumerate(head)}
	file.close()
	return data

d = parseFile('../output_data/decay.dat')
dk = list(d.keys())

for i in range(len(dk)-1):
	plt.semilogx(d['time'],d[dk[i+1]], label=dk[i+1])

plt.grid()
plt.legend()
plt.ylabel('Abundance')
plt.xlabel('Time')
plt.tight_layout()
plt.savefig('../figures/rad_decay_class.pdf', dpi=1096)
plt.close('all')
