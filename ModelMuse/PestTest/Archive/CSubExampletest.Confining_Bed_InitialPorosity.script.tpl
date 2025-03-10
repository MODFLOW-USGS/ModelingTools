ptf @
#Script for PLPROC

#Read parameter values
a = @                        a@
# Pilot points are not used with a.

#Read MODFLOW 6 grid information file
cl_Discretization1 = read_mf6_grid_specs(file='CSubExampletest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id1; layer=1, &
  plist_layer_bottom =bot1; layer=1, &
  plist_top = top)
cl_Discretization2 = read_mf6_grid_specs(file='CSubExampletest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id2; layer=2, &
  plist_layer_bottom =bot2; layer=2, &
  )
cl_Discretization3 = read_mf6_grid_specs(file='CSubExampletest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id3; layer=3, &
  plist_layer_bottom =bot3; layer=3, &
  )
cl_Discretization4 = read_mf6_grid_specs(file='CSubExampletest.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id4; layer=4, &
  plist_layer_bottom =bot4; layer=4, &
  )



# Layer     1

#Read data to modify
read_list_file(reference_clist='cl_Discretization1',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='CSubExampletest.Confining_Bed_InitialPorosity.PstValues')

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization1,value=0.0)
# Setting values for layer     1
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * a

#Write new data values
write_column_data_file(header='no', &
  file='arrays\CSubExampletest.Confining_Bed_InitialPorosity_1.arrays';delim="space", &
  plist=p_Value1)

# Remove sLists and pLists
s_PIndex1.remove()
p_Value1.remove()
temp1.remove()

# Layer     2

#Read data to modify
read_list_file(reference_clist='cl_Discretization2',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='CSubExampletest.Confining_Bed_InitialPorosity.PstValues')

# Modfify data values
temp2=new_plist(reference_clist=cl_Discretization2,value=0.0)
# Setting values for layer     2
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value2(select=(s_PIndex2 == 1)) = p_Value2 * a

#Write new data values
write_column_data_file(header='no', &
  file='arrays\CSubExampletest.Confining_Bed_InitialPorosity_2.arrays';delim="space", &
  plist=p_Value2)

# Remove sLists and pLists
s_PIndex2.remove()
p_Value2.remove()
temp2.remove()

# Layer     3

#Read data to modify
read_list_file(reference_clist='cl_Discretization3',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='CSubExampletest.Confining_Bed_InitialPorosity.PstValues')

# Modfify data values
temp3=new_plist(reference_clist=cl_Discretization3,value=0.0)
# Setting values for layer     3
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value3(select=(s_PIndex3 == 1)) = p_Value3 * a

#Write new data values
write_column_data_file(header='no', &
  file='arrays\CSubExampletest.Confining_Bed_InitialPorosity_3.arrays';delim="space", &
  plist=p_Value3)

# Remove sLists and pLists
s_PIndex3.remove()
p_Value3.remove()
temp3.remove()

# Layer     4

#Read data to modify
read_list_file(reference_clist='cl_Discretization4',skiplines=1, &
  slist=s_PIndex4;column=8, &
  plist=p_Value4;column=9, &
  file='CSubExampletest.Confining_Bed_InitialPorosity.PstValues')

# Modfify data values
temp4=new_plist(reference_clist=cl_Discretization4,value=0.0)
# Setting values for layer     4
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value4(select=(s_PIndex4 == 1)) = p_Value4 * a

#Write new data values
write_column_data_file(header='no', &
  file='arrays\CSubExampletest.Confining_Bed_InitialPorosity_4.arrays';delim="space", &
  plist=p_Value4)

# Remove sLists and pLists
s_PIndex4.remove()
p_Value4.remove()
temp4.remove()

