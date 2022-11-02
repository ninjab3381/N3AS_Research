import math
import matplotlib.pyplot as plt

#choice_dict = {1:'H3', 2: 'HeH'}
#filename1 = 'new123_top_only.dat'
#filename2 = 'new123_bottom_only.dat'


filename1 = 'fort.9'


np_rate_fwd_lst = []
np_rate_rv_lst = []
temp_lst = []



with open(filename1, 'r') as f:
    for line in f:
        fields = line.split()
        np_rate_fwd = float(fields[1].strip())
        np_rate_rv = float(fields[2].strip())
        temp = float(fields[0].strip())
        np_rate_fwd_lst.append(np_rate_fwd)
        np_rate_rv_lst.append(np_rate_rv)
        temp_lst.append(temp)


fig, ax = plt.subplots()
fig.set_figheight(12)
fig.set_figwidth(10)


ax.invert_xaxis()
ax.plot(temp_lst, np_rate_fwd_lst)
ax.set_title('np rate fwd vs temp')
ax.set_xlabel('temp')
ax.set_ylabel('np rate fwd')

#fig.subplots_adjust(hspace=.5)

plt.show()


fig, ax = plt.subplots()
fig.set_figheight(12)
fig.set_figwidth(10)


ax.invert_xaxis()
ax.plot(temp_lst, np_rate_rv_lst)
ax.set_title('np rate rv vs temp')
ax.set_xlabel('temp')
ax.set_ylabel('np rate rv')

#fig.subplots_adjust(hspace=.5)

plt.show()




