ptf $
#Script for PLPROC

#Read SUTRA node information file
cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  plist=p_x;column=2, &
  plist=p_y;column=3, &
  slist=s_NN2D;column=4, &
  id_type='indexed',file='Rocky2D_IrregularS4NoInterpolate.c_nod')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z1;column=5, &
  slist=s_NN3D1;column=6, &
  slist=s_Active_1;column=7, &
  id_type='indexed',file='Rocky2D_IrregularS4NoInterpolate.c_nod')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z2;column=8, &
  slist=s_NN3D2;column=9, &
  slist=s_Active_2;column=10, &
  id_type='indexed',file='Rocky2D_IrregularS4NoInterpolate.c_nod')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z3;column=11, &
  slist=s_NN3D3;column=12, &
  slist=s_Active_3;column=13, &
  id_type='indexed',file='Rocky2D_IrregularS4NoInterpolate.c_nod')

#Read pilot point data

# Read data to modify
# Read porosity
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Layer1;column=3, &
  plist=p_Porosity1;column=4, &
  slist=s_PorPar1;column=5, &
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Layer2;column=7, &
  plist=p_Porosity2;column=8, &
  slist=s_PorPar2;column=9, &
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Layer3;column=11, &
  plist=p_Porosity3;column=12, &
  slist=s_PorPar3;column=13, &
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity')

# Read Unsaturated Zone
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Unsat_Region1;column=4, &
  file='Rocky2D_IrregularS4NoInterpolate.Region_Nodes')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Unsat_Region2;column=7, &
  file='Rocky2D_IrregularS4NoInterpolate.Region_Nodes')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Unsat_Region3;column=10, &
  file='Rocky2D_IrregularS4NoInterpolate.Region_Nodes')

# Read COMPMA
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Matrix_Compressibility')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_COMPMA1;column=4, &
  slist=s_COMPMAPar1;column=5, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Matrix_Compressibility')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_COMPMA2;column=8, &
  slist=s_COMPMAPar2;column=9, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Matrix_Compressibility')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_COMPMA3;column=12, &
  slist=s_COMPMAPar3;column=13, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Matrix_Compressibility')

# Read CS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Specific_Heat')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_CS1;column=4, &
  slist=s_CSPar1;column=5, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Specific_Heat')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_CS2;column=8, &
  slist=s_CSPar2;column=9, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Specific_Heat')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_CS3;column=12, &
  slist=s_CSPar3;column=13, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Specific_Heat')

# Read RHOS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Density')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_RHOS1;column=4, &
  slist=s_RHOSPar1;column=5, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Density')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_RHOS2;column=8, &
  slist=s_RHOSPar2;column=9, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Density')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_RHOS3;column=12, &
  slist=s_RHOSPar3;column=13, &
  file='Rocky2D_IrregularS4NoInterpolate.Solid_Grain_Density')

#Read parameter values
Density = $                        Density$
#Density = 1.1

temp=new_plist(reference_clist=cl_Discretization,value=0.0)
# applying parameter values
# applying parameter Density
    # Substituting parameter values in zones
p_Porosity1(select=(s_PorPar1 == 1)) = p_Porosity1 * Density
    # Substituting parameter values in zones
p_Porosity2(select=(s_PorPar2 == 1)) = p_Porosity2 * Density
    # Substituting parameter values in zones
p_Porosity3(select=(s_PorPar3 == 1)) = p_Porosity3 * Density

    # Substituting parameter values in zones
p_COMPMA1(select=(s_COMPMAPar1 == 1)) = p_COMPMA1 * Density
    # Substituting parameter values in zones
p_COMPMA2(select=(s_COMPMAPar2 == 1)) = p_COMPMA2 * Density
    # Substituting parameter values in zones
p_COMPMA3(select=(s_COMPMAPar3 == 1)) = p_COMPMA3 * Density

    # Substituting parameter values in zones
p_CS1(select=(s_CSPar1 == 1)) = p_CS1 * Density
    # Substituting parameter values in zones
p_CS2(select=(s_CSPar2 == 1)) = p_CS2 * Density
    # Substituting parameter values in zones
p_CS3(select=(s_CSPar3 == 1)) = p_CS3 * Density

    # Substituting parameter values in zones
p_RHOS1(select=(s_RHOSPar1 == 1)) = p_RHOS1 * Density
    # Substituting parameter values in zones
p_RHOS2(select=(s_RHOSPar2 == 1)) = p_RHOS2 * Density
    # Substituting parameter values in zones
p_RHOS3(select=(s_RHOSPar3 == 1)) = p_RHOS3 * Density

# Write new data values
write_column_data_file(header = 'no', &
  file='Rocky2D_IrregularS4NoInterpolate.14B_1';delim="space", &
  select=(s_Active_1 == 1), &
  slist='s_NN3D1', &
  slist=s_Unsat_Region1, &
  plist=p_x, &
  plist=p_y, &
  plist=p_z1, &
  plist=p_Porosity1, &
  plist=p_COMPMA1, &
  plist=p_CS1, &
  plist=p_RHOS1)
write_column_data_file(header = 'no', &
  file='Rocky2D_IrregularS4NoInterpolate.14B_2';delim="space", &
  select=(s_Active_2 == 1), &
  slist='s_NN3D2', &
  slist=s_Unsat_Region2, &
  plist=p_x, &
  plist=p_y, &
  plist=p_z2, &
  plist=p_Porosity2, &
  plist=p_COMPMA2, &
  plist=p_CS2, &
  plist=p_RHOS2)
write_column_data_file(header = 'no', &
  file='Rocky2D_IrregularS4NoInterpolate.14B_3';delim="space", &
  select=(s_Active_3 == 1), &
  slist='s_NN3D3', &
  slist=s_Unsat_Region3, &
  plist=p_x, &
  plist=p_y, &
  plist=p_z3, &
  plist=p_Porosity3, &
  plist=p_COMPMA3, &
  plist=p_CS3, &
  plist=p_RHOS3)
