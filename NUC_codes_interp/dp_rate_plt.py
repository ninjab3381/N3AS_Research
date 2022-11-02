import math
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams['text.usetex'] = True

#choice_dict = {1:'H3', 2: 'HeH'}
#filename1 = 'new123_top_only.dat'
#filename2 = 'new123_bottom_only.dat'

filename1 = 'NUC_codes_interp/sigma_v_d(p,g)3He_gauss.dat'
filename2 = 'NUC_codes_interp/new_sigma_v_d(p,g)3He.dat'


dp_rate_lst = []
temp_lst = []
dp_rate_lst_2 = []


with open(filename1, 'r') as f:
    next(f) #since temperature starts at 0 for some reason...
    for line in f:
        fields = line.split()
        temp = float(fields[0].strip())
        dp_rate = float(fields[1].strip())
        dp_rate_lst.append(dp_rate)
        temp_lst.append(temp)

with open(filename2, 'r') as f:
    next(f) #since temperature starts at 0 for some reason...
    for line in f:
        fields = line.split()
        dp_rate_2 = float(fields[1].strip())
        dp_rate_lst_2.append(dp_rate_2)



fig, ax = plt.subplots()
fig.set_figheight(12)
fig.set_figwidth(10)


ax.invert_xaxis()
ax.loglog(temp_lst, dp_rate_lst, label = r'gauss')



ax.loglog(temp_lst, dp_rate_lst_2, label = r'kawano')

ax.legend()

ax.set_title('dp rate vs temp')
ax.set_xlabel('temp')
ax.set_ylabel('dp rate')

#fig.subplots_adjust(hspace=.5)

plt.savefig('plt_gauss.pdf')






