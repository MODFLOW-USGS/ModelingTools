ptf @
#Script for PLPROC

cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  id_type='indexed',file='C:\ModelingTools\ModelMuse\PestTest\SutraGenBoundLakeTest.c_nod')
#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='SutraGenBoundLakeTest.TestDS.PstValues')

#Read parameter values
a = @                        a@
# Pilot points are not used with a.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * a

#Write new data values
write_column_data_file(header='no', &
  file='arrays\SutraGenBoundLakeTest.TestDS_1.arrays';delim="space", &
  plist=p_Value1)
