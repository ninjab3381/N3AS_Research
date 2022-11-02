import math
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams['text.usetex'] = True

#choice_dict = {1:'H3', 2: 'HeH'}
#filename1 = 'new123_top_only.dat'
#filename2 = 'new123_bottom_only.dat'

filename1 = 'fort.10'
filename2 = 'fort.9'
filename3 = 'fort.11'


np_rate_lst = []
temp_lst = []
np_rate_lst_2 = []
temp_lst_2 = []


with open(filename1, 'r') as f:
    next(f) #since temperature starts at 0 for some reason...
    for line in f:
        fields = line.split()
        np_rate = float(fields[0].strip())
        temp = float(fields[1].strip())
        np_rate_lst.append(np_rate)
        temp_lst.append(temp)

with open(filename3, 'r') as f:
    next(f) #since temperature starts at 0 for some reason...
    for line in f:
        fields = line.split()
        np_rate_2 = float(fields[0].strip())
        temp_2 = float(fields[1].strip())
        np_rate_lst_2.append(np_rate_2)
        temp_lst_2.append(temp_2)


fig, ax = plt.subplots()
fig.set_figheight(12)
fig.set_figwidth(10)


ax.invert_xaxis()
ax.loglog(temp_lst, np_rate_lst, label = r'$n\rightarrow p$')



ax.loglog(temp_lst_2, np_rate_lst_2, label = r'$p\rightarrow n$')

ax.legend()

ax.set_title('np rate vs temp')
ax.set_xlabel('temp')
ax.set_ylabel('np rate')

#fig.subplots_adjust(hspace=.5)

plt.savefig('plt.pdf')






