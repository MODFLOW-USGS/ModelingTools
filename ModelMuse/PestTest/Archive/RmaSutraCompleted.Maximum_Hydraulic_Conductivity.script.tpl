ptf $
#Script for PLPROC

#Read pilot point data
PMXPilotPoints1 = read_list_file(skiplines=0,dimensions=2, &
  plist='K_1';column=5, &
  id_type='character',file='RmaSutraCompleted.Maximum_Hydraulic_Conductivity.K.1.pp')

cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  id_type='indexed',file='RmaSutraCompleted.c_ele')

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='RmaSutraCompleted.Maximum_Hydraulic_Conductivity.PstValues')

#Read parameter values
K = $                        K$
# Pilot points are used with K.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter K
    # Substituting interpolated values
    # Get interpolated values
    temp1=K_1.krige_using_file(file='RmaSutraCompleted.Maximum_Hydraulic_Conductivity.Factors1';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value1(select=(s_PIndex1 == 1)) = temp1

#Write new data values
write_column_data_file(header='no', &
  file='arrays\RmaSutraCompleted.Maximum_Hydraulic_Conductivity_1.arrays';delim="space", &
  plist=p_Value1)