
def fix_prog(data):
    i = 0
    while(i < len(data)):
        if(data[i] == '1'):
            e1 = data[int(data[i+1])]
            e2 = data[int(data[i+2])]
            data[int(data[i+3])] = str(int(e1) + int(e2))
            i = i + 4
            pass
        elif(data[i] == '2'):
            data[int(data[i+3])] = str(
                int(data[int(data[i+1])]) *
                int(data[int(data[i+2])]))
            i = i + 4
            pass
        elif(data[i] == '99'):
            return data
        else:
            pass

def read_file(fn):
    f = open(fn, 'r')
    line = f.readline()
    data = line.split(',')
    return data

def find_val(data):
    i = 0
    j = 0
    while(i < 117):
        while(j < 117):
            t = data.copy()
            print(i,j)
            t[1] = str(i)
            t[2] = str(j)
            print(t)
            t2 = fix_prog(t)
            if(int(t2[0]) == 19690720):
                return(i,j)
            j = j + 1
        j = 0
        i = i + 1
            
