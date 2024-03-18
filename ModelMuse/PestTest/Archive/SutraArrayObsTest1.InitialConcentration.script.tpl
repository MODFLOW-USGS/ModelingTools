ptf $
#Script for PLPROC

#Read parameter values
a = $                        a$
#a = 1
conc1 = $                        conc1$
#conc1 = 0.1
conc2 = $                        conc2$
#conc2 = 0.2
K = $                        K$
#K = 1

#Read SUTRA node information file
cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  plist=p_x;column=2, &
  plist=p_y;column=3, &
  slist=s_NN2D;column=4, &
  id_type='indexed',file='SutraArrayObsTest1.c_nod')
temp=new_plist(reference_clist=cl_Discretization,value=0.0)

# Layer     1

#Read pilot point data

#Read data to modify
# Read InitialConcentration
temp.remove()
temp=new_plist(reference_clist=cl_Discretization,value=0.0)

read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_Data1;column=2, &
  slist=s_DataPar1;column=3, &
  file='SutraArrayObsTest1.InitialConcentration')

# applying parameter values
# applying parameter a
    # Substituting parameter values in zones
p_Data1(select=(s_DataPar1 == 1)) = p_Data1 * a

# applying parameter conc1
    # Substituting parameter values in zones
p_Data1(select=(s_DataPar1 == 2)) = p_Data1 * conc1

# applying parameter conc2
    # Substituting parameter values in zones
p_Data1(select=(s_DataPar1 == 3)) = p_Data1 * conc2

# applying parameter K
    # Substituting parameter values in zones
p_Data1(select=(s_DataPar1 == 4)) = p_Data1 * K

s_DataPar1.remove()

# Write new data values
write_column_data_file(header = 'no', &
  file='SutraArrayObsTest1.UVEC';delim="space", &
  plist=p_Data1)
