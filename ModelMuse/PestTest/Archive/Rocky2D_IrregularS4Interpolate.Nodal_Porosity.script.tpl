ptf $
#Script for PLPROC

#Read pilot point data
PilotPoints1 = read_list_file(skiplines=0,dimensions=2, &
  plist='Density_1';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.Density.1.pp')
PilotPoints2 = read_list_file(skiplines=0,dimensions=2, &
  plist='Density_2';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.Density.2.pp')
PilotPoints3 = read_list_file(skiplines=0,dimensions=2, &
  plist='Density_3';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.Density.3.pp')

cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  id_type='indexed',file='Rocky2D_IrregularS4Interpolate.c_nod')

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.PstValues')

#Read parameter values
Density = $                        Density$
# Pilot points are used with Density.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter Density
    # Substituting interpolated values
    # Get interpolated values
    temp1=Density_1.krige_using_file(file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.Factors1';form='formatted', &
      transform='none')
    # Write interpolated values in zones
    p_Value1(select=(s_PIndex1 == 1)) = temp1
temp2=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     2
  # Setting values for parameter Density
    # Substituting interpolated values
    # Get interpolated values
    temp2=Density_2.krige_using_file(file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.Factors2';form='formatted', &
      transform='none')
    # Write interpolated values in zones
    p_Value2(select=(s_PIndex2 == 1)) = temp2
temp3=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     3
  # Setting values for parameter Density
    # Substituting interpolated values
    # Get interpolated values
    temp3=Density_3.krige_using_file(file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity.Factors3';form='formatted', &
      transform='none')
    # Write interpolated values in zones
    p_Value3(select=(s_PIndex3 == 1)) = temp3

#Write new data values
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4Interpolate.Nodal_Porosity_1.arrays';delim="space", &
  plist=p_Value1)
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4Interpolate.Nodal_Porosity_2.arrays';delim="space", &
  plist=p_Value2)
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4Interpolate.Nodal_Porosity_3.arrays';delim="space", &
  plist=p_Value3)
