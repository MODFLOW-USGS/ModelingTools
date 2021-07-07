ptf @
#Script for PLPROC

#Read MODFLOW-2005 grid information file
cl_Discretization = read_mf_grid_specs(file="C:\ModelingTools\ModelMuse\PestTest\ResTest.gsf")#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='ResTest.Reservoir_Bed_Thickness.PstValues')

#Read parameter values
ResArr = @                        ResArr@
# Pilot points are not used with ResArr.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter ResArr
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * ResArr

#Write new data values
write_column_data_file(header='no', &
  file='arrays\ResTest.Reservoir_Bed_Thickness_1.arrays';delim="space", &
  plist=p_Value1)
