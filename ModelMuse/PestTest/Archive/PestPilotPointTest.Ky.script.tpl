ptf @
#Script for PLPROC

#Read pilot point data
PilotPoints1 = read_list_file(skiplines=0,dimensions=2, &
  plist='Hk1_1';column=5, &
  id_type='character',file='PestPilotPointTest.Ky.Hk1.1.pp')
PilotPoints3 = read_list_file(skiplines=0,dimensions=2, &
  plist='Hk1_2';column=5, &
  id_type='character',file='PestPilotPointTest.Ky.Hk1.2.pp')
PilotPoints5 = read_list_file(skiplines=0,dimensions=2, &
  plist='Hk1_3';column=5, &
  id_type='character',file='PestPilotPointTest.Ky.Hk1.3.pp')
PilotPoints2 = read_list_file(skiplines=0,dimensions=2, &
  plist='Hk2_1';column=5, &
  id_type='character',file='PestPilotPointTest.Ky.Hk2.1.pp')
PilotPoints4 = read_list_file(skiplines=0,dimensions=2, &
  plist='Hk2_2';column=5, &
  id_type='character',file='PestPilotPointTest.Ky.Hk2.2.pp')
PilotPoints6 = read_list_file(skiplines=0,dimensions=2, &
  plist='Hk2_3';column=5, &
  id_type='character',file='PestPilotPointTest.Ky.Hk2.3.pp')

#Read MODFLOW 6 grid information file
cl_Discretization1 = read_mf6_grid_specs(file='PestPilotPointTest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id1; layer=1, &
  plist_layer_bottom =bot1; layer=1, &
  plist_top = top)
cl_Discretization2 = read_mf6_grid_specs(file='PestPilotPointTest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id2; layer=2, &
  plist_layer_bottom =bot2; layer=2, &
  )
cl_Discretization3 = read_mf6_grid_specs(file='PestPilotPointTest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id3; layer=3, &
  plist_layer_bottom =bot3; layer=3, &
  )


#Read data to modify
read_list_file(reference_clist='cl_Discretization1',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='PestPilotPointTest.Ky.PstValues')
read_list_file(reference_clist='cl_Discretization2',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='PestPilotPointTest.Ky.PstValues')
read_list_file(reference_clist='cl_Discretization3',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='PestPilotPointTest.Ky.PstValues')

#Read parameter values
Hk1 = @                        Hk1@
# Pilot points are used with Hk1.
Hk2 = @                        Hk2@
# Pilot points are used with Hk2.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization1,value=0.0)
# Setting values for layer     1
  # Setting values for parameter Hk1
    # Substituting interpolated values
    # Get interpolated values
    temp1=Hk1_1.krige_using_file(file='PestPilotPointTest.Ky.Factors1';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value1(select=(s_PIndex1 == 1)) = temp1
  # Setting values for parameter Hk2
    # Substituting interpolated values
    # Get interpolated values
    temp1=Hk2_1.krige_using_file(file='PestPilotPointTest.Ky.Factors2';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value1(select=(s_PIndex1 == 2)) = temp1
temp2=new_plist(reference_clist=cl_Discretization2,value=0.0)
# Setting values for layer     2
  # Setting values for parameter Hk1
    # Substituting interpolated values
    # Get interpolated values
    temp2=Hk1_2.krige_using_file(file='PestPilotPointTest.Ky.Factors3';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value2(select=(s_PIndex2 == 1)) = temp2
  # Setting values for parameter Hk2
    # Substituting interpolated values
    # Get interpolated values
    temp2=Hk2_2.krige_using_file(file='PestPilotPointTest.Ky.Factors4';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value2(select=(s_PIndex2 == 2)) = temp2
temp3=new_plist(reference_clist=cl_Discretization3,value=0.0)
# Setting values for layer     3
  # Setting values for parameter Hk1
    # Substituting interpolated values
    # Get interpolated values
    temp3=Hk1_3.krige_using_file(file='PestPilotPointTest.Ky.Factors5';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value3(select=(s_PIndex3 == 1)) = temp3
  # Setting values for parameter Hk2
    # Substituting interpolated values
    # Get interpolated values
    temp3=Hk2_3.krige_using_file(file='PestPilotPointTest.Ky.Factors6';form='formatted', &
      transform='log')
    # Write interpolated values in zones
    p_Value3(select=(s_PIndex3 == 2)) = temp3

#Write new data values
write_column_data_file(header='no', &
  file='arrays\PestPilotPointTest.Ky_1.arrays';delim="space", &
  plist=p_Value1)
write_column_data_file(header='no', &
  file='arrays\PestPilotPointTest.Ky_2.arrays';delim="space", &
  plist=p_Value2)
write_column_data_file(header='no', &
  file='arrays\PestPilotPointTest.Ky_3.arrays';delim="space", &
  plist=p_Value3)
