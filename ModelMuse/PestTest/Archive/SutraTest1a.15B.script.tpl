ptf $
#Script for PLPROC

#Read SUTRA element information file
cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  plist=p_x;column=2, &
  plist=p_y;column=3, &
  plist=p_EN2D;column=4, &
  id_type='indexed',file='SutraTest1a.c_ele')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z1;column=5, &
  slist=s_EN3D1;column=6, &
  slist=s_Active_1;column=7, &
  id_type='indexed',file='SutraTest1a.c_ele')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z2;column=8, &
  slist=s_EN3D2;column=9, &
  slist=s_Active_2;column=10, &
  id_type='indexed',file='SutraTest1a.c_ele')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z3;column=11, &
  slist=s_EN3D3;column=12, &
  slist=s_Active_3;column=13, &
  id_type='indexed',file='SutraTest1a.c_ele')


#Read data to modify
# Read Unsaturated Zone
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_LREG1;column=4, &
  file='SutraTest1a.Region_Elements')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_LREG2;column=7, &
  file='SutraTest1a.Region_Elements')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_LREG3;column=10, &
  file='SutraTest1a.Region_Elements')

# Read Maximum_Permeability
  p_PMAX1=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMAX1.read_list_as_array(file='arrays\SutraTest1a.Maximum_Permeability_1.arrays')
  s_PMAXPar1=new_slist(reference_clist='cl_Discretization',value=1)
  p_PMAX2=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMAX2.read_list_as_array(file='arrays\SutraTest1a.Maximum_Permeability_2.arrays')
  s_PMAXPar2=new_slist(reference_clist='cl_Discretization',value=2)
  p_PMAX3=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMAX3.read_list_as_array(file='arrays\SutraTest1a.Maximum_Permeability_3.arrays')
  s_PMAXPar3=new_slist(reference_clist='cl_Discretization',value=3)

# Read Middle_Permeability
  p_PMID1=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMID1.read_list_as_array(file='arrays\SutraTest1a.Middle_Permeability_1.arrays')
  s_PMIDPar1=new_slist(reference_clist='cl_Discretization',value=1)
  p_PMID2=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMID2.read_list_as_array(file='arrays\SutraTest1a.Middle_Permeability_2.arrays')
  s_PMIDPar2=new_slist(reference_clist='cl_Discretization',value=2)
  p_PMID3=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMID3.read_list_as_array(file='arrays\SutraTest1a.Middle_Permeability_3.arrays')
  s_PMIDPar3=new_slist(reference_clist='cl_Discretization',value=3)

# Read Minimum_Permeability
  p_PMIN1=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMIN1.read_list_as_array(file='arrays\SutraTest1a.Minimum_Permeability_1.arrays')
  s_PMINPar1=new_slist(reference_clist='cl_Discretization',value=1)
  p_PMIN2=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMIN2.read_list_as_array(file='arrays\SutraTest1a.Minimum_Permeability_2.arrays')
  s_PMINPar2=new_slist(reference_clist='cl_Discretization',value=2)
  p_PMIN3=new_plist(reference_clist='cl_Discretization',value=1.0)
  p_PMIN3.read_list_as_array(file='arrays\SutraTest1a.Minimum_Permeability_3.arrays')
  s_PMINPar3=new_slist(reference_clist='cl_Discretization',value=3)

# Read Angle_Horizontal
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE11;column=4, &
  slist=s_ANGLE1Par1;column=5, &
  file='SutraTest1a.Angle_Horizontal')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE12;column=8, &
  slist=s_ANGLE1Par2;column=9, &
  file='SutraTest1a.Angle_Horizontal')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE13;column=12, &
  slist=s_ANGLE1Par3;column=13, &
  file='SutraTest1a.Angle_Horizontal')

# Read Angle_Vertical
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE21;column=4, &
  slist=s_ANGLE2Par1;column=5, &
  file='SutraTest1a.Angle_Vertical')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE22;column=8, &
  slist=s_ANGLE2Par2;column=9, &
  file='SutraTest1a.Angle_Vertical')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE23;column=12, &
  slist=s_ANGLE2Par3;column=13, &
  file='SutraTest1a.Angle_Vertical')

# Read Angle_Rotational
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE31;column=4, &
  slist=s_ANGLE3Par1;column=5, &
  file='SutraTest1a.Angle_Rotational')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE32;column=8, &
  slist=s_ANGLE3Par2;column=9, &
  file='SutraTest1a.Angle_Rotational')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ANGLE33;column=12, &
  slist=s_ANGLE3Par3;column=13, &
  file='SutraTest1a.Angle_Rotational')

# Read Longitudinal_Dispersivity_Max_Dir
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMAX1;column=4, &
  slist=s_ALMAXPar1;column=5, &
  file='SutraTest1a.Longitudinal_Dispersivity_Max_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMAX2;column=8, &
  slist=s_ALMAXPar2;column=9, &
  file='SutraTest1a.Longitudinal_Dispersivity_Max_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMAX3;column=12, &
  slist=s_ALMAXPar3;column=13, &
  file='SutraTest1a.Longitudinal_Dispersivity_Max_Dir')

# Read Longitudinal_Dispersivity_Mid_Dir
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMID1;column=4, &
  slist=s_ALMIDPar1;column=5, &
  file='SutraTest1a.Longitudinal_Dispersivity_Mid_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMID2;column=8, &
  slist=s_ALMIDPar2;column=9, &
  file='SutraTest1a.Longitudinal_Dispersivity_Mid_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMID3;column=12, &
  slist=s_ALMIDPar3;column=13, &
  file='SutraTest1a.Longitudinal_Dispersivity_Mid_Dir')

# Read Longitudinal_Dispersivity_Min_Dir
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMIN1;column=4, &
  slist=s_ALMINPar1;column=5, &
  file='SutraTest1a.Longitudinal_Dispersivity_Min_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMIN2;column=8, &
  slist=s_ALMINPar2;column=9, &
  file='SutraTest1a.Longitudinal_Dispersivity_Min_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ALMIN3;column=12, &
  slist=s_ALMINPar3;column=13, &
  file='SutraTest1a.Longitudinal_Dispersivity_Min_Dir')

# Read Transverse_Dispersivity_Max_Dir
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMAX1;column=4, &
  slist=s_ATMAXPar1;column=5, &
  file='SutraTest1a.Transverse_Dispersivity_Max_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMAX2;column=8, &
  slist=s_ATMAXPar2;column=9, &
  file='SutraTest1a.Transverse_Dispersivity_Max_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMAX3;column=12, &
  slist=s_ATMAXPar3;column=13, &
  file='SutraTest1a.Transverse_Dispersivity_Max_Dir')

# Read Transverse_Dispersivity_Mid_Dir
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMID1;column=4, &
  slist=s_ATMIDPar1;column=5, &
  file='SutraTest1a.Transverse_Dispersivity_Mid_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMID2;column=8, &
  slist=s_ATMIDPar2;column=9, &
  file='SutraTest1a.Transverse_Dispersivity_Mid_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMID3;column=12, &
  slist=s_ATMIDPar3;column=13, &
  file='SutraTest1a.Transverse_Dispersivity_Mid_Dir')

# Read Transverse_Dispersivity_Min_Dir
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMIN1;column=4, &
  slist=s_ATMINPar1;column=5, &
  file='SutraTest1a.Transverse_Dispersivity_Min_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMIN2;column=8, &
  slist=s_ATMINPar2;column=9, &
  file='SutraTest1a.Transverse_Dispersivity_Min_Dir')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_ATMIN3;column=12, &
  slist=s_ATMINPar3;column=13, &
  file='SutraTest1a.Transverse_Dispersivity_Min_Dir')

#Read parameter values
a = $                        a$
#a = 1.1
conc1 = $                        conc1$
#conc1 = 0.1
conc2 = $                        conc2$
#conc2 = 0.2

# applying parameter values
temp=new_plist(reference_clist=cl_Discretization,value=0.0)
# applying parameter a

# applying parameter conc1

# applying parameter conc2

# Write new data values
write_column_data_file(header = 'no', &
  file='SutraTest1a.15B_1';delim="space", &
  select=(s_Active_1 == 1), &
  slist='s_EN3D1', &
  slist=s_LREG1, &
  plist=p_PMAX1, &
  plist=p_PMID1, &
  plist=p_PMIN1, &
  plist=p_ANGLE11, &
  plist=p_ANGLE21, &
  plist=p_ANGLE31, &
  plist=p_ALMAX1, &
  plist=p_ALMID1, &
  plist=p_ALMIN1, &
  plist=p_ATMAX1, &
  plist=p_ATMID1, &
  plist=p_ATMIN1)
write_column_data_file(header = 'no', &
  file='SutraTest1a.15B_2';delim="space", &
  select=(s_Active_2 == 1), &
  slist='s_EN3D2', &
  slist=s_LREG2, &
  plist=p_PMAX2, &
  plist=p_PMID2, &
  plist=p_PMIN2, &
  plist=p_ANGLE12, &
  plist=p_ANGLE22, &
  plist=p_ANGLE32, &
  plist=p_ALMAX2, &
  plist=p_ALMID2, &
  plist=p_ALMIN2, &
  plist=p_ATMAX2, &
  plist=p_ATMID2, &
  plist=p_ATMIN2)
write_column_data_file(header = 'no', &
  file='SutraTest1a.15B_3';delim="space", &
  select=(s_Active_3 == 1), &
  slist='s_EN3D3', &
  slist=s_LREG3, &
  plist=p_PMAX3, &
  plist=p_PMID3, &
  plist=p_PMIN3, &
  plist=p_ANGLE13, &
  plist=p_ANGLE23, &
  plist=p_ANGLE33, &
  plist=p_ALMAX3, &
  plist=p_ALMID3, &
  plist=p_ALMIN3, &
  plist=p_ATMAX3, &
  plist=p_ATMID3, &
  plist=p_ATMIN3)
