{@name is used to convert coordinates in latitude and longitude expressed in
  radians to X and Y coordinates expressed in the Universal Transverse Mercator
  (UTM) projection.  See @link(ConvertToUTM).

  For more information, see
  Snyder, J.P., 1987, Map Projections—A Working Manual,
  U.S. Geological Survey Professional Paper 1395, 383 p.}
unit CoordinateConversionUnit;

interface

type
  {@name defines an ellipsoid used for converting from latitude and longitude
    to UTM coordinates.  See @link(ConvertToUTM).}
  TEllipsoid = record
    // Equatorial radius in meters
    // (Snyder, 1987, p. viii and p. 16.)
    LowerCase_a: double;
    // eccentricity squared = 2f - f^2
    // where f = flattening.
    // (Snyder, 1987, p. viii and p. 13.)
    e_square: double;
    // central scale factor
    // (Snyder, 1987, p. ix and p. 23.)
    k_zero: double;
    // e_square/(1-e_square)
    // (Snyder, 1987, p. 61.)
    e_prime_square: double;
  end;

{@name is converts coordinates in latitude and longitude expressed in
  radians to X and Y coordinates expressed in the Universal Transverse Mercator
  (UTM) projection.  CentralMeridianInRadians is the central meridian of the
  zone UTM grid zone.}
procedure ConvertToUTM(const LatitudeRadians,
  LongitudeRadians, CentralMeridianInRadians: double;
  out X, Y: double);

const
  // @abstract(From Snyder, 1987.)
  // See @link(Ellipsoid)
  Clarke1866: TEllipsoid =
    (LowerCase_a: 6378206.4;
    e_square: 0.00676866;
    k_zero: 0.9996;
    e_prime_square: 0.00676866/(1-0.00676866));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Airy1830: TEllipsoid =
    (LowerCase_a: 6377563.396;
    e_square: 0.00667054;
    k_zero: 0.9996;
    e_prime_square: 0.00667054/(1-0.00667054));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Bessel1841: TEllipsoid =
    (LowerCase_a: 6377397.155;
    e_square: 0.006674372;
    k_zero: 0.9996;
    e_prime_square: 0.006674372/(1-0.006674372));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Clarke1880: TEllipsoid =
    (LowerCase_a: 6378249.145;
    e_square: 0.006803511;
    k_zero: 0.9996;
    e_prime_square: 0.006803511/(1-0.006803511));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Everest1830: TEllipsoid =
    (LowerCase_a: 6377276.345;
    e_square: 0.006637847;
    k_zero: 0.9996;
    e_prime_square: 0.006637847/(1-0.006637847));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Fischer1960: TEllipsoid =
    (LowerCase_a: 638166.0;
    e_square: 0.006693422;
    k_zero: 0.9996;
    e_prime_square: 0.006693422/(1-0.006693422));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Fischer1968: TEllipsoid =
    (LowerCase_a: 638150.0;
    e_square: 0.006693422;
    k_zero: 0.9996;
    e_prime_square: 0.006693422/(1-0.006693422));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  GRS67_1967: TEllipsoid =
    (LowerCase_a: 6378160.0;
    e_square: 0.006694605;
    k_zero: 0.9996;
    e_prime_square: 0.006694605/(1-0.006694605));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  GRS75_1975: TEllipsoid =
    (LowerCase_a: 6378140.0;
    e_square: 0.006694385;
    k_zero: 0.9996;
    e_prime_square: 0.006694385/(1-0.006694385));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  GRS80_1980: TEllipsoid =
    (LowerCase_a: 6378137.0;
    e_square: 0.00669438;
    k_zero: 0.9996;
    e_prime_square: 0.00669438/(1-0.00669438));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Hough1956: TEllipsoid =
    (LowerCase_a: 6378270.0;
    e_square: 0.00672267;
    k_zero: 0.9996;
    e_prime_square: 0.00672267/(1-0.00672267));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  International1924: TEllipsoid =
    (LowerCase_a: 6378388.0;
    e_square: 0.00672267;
    k_zero: 0.9996;
    e_prime_square: 0.00672267/(1-0.00672267));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  Krassowsky1940: TEllipsoid =
    (LowerCase_a: 6378245.0;
    e_square: 0.006693422;
    k_zero: 0.9996;
    e_prime_square: 0.006693422/(1-0.006693422));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  SouthAmerican1969: TEllipsoid =
    (LowerCase_a: 6378160.0;
    e_square: 0.006694542;
    k_zero: 0.9996;
    e_prime_square: 0.006694542/(1-0.006694542));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  WGS60_1960: TEllipsoid =
    (LowerCase_a: 6378165.0;
    e_square: 0.006693422;
    k_zero: 0.9996;
    e_prime_square: 0.006693422/(1-0.006693422));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  WGS66_1966: TEllipsoid =
    (LowerCase_a: 6378145;
    e_square: 0.006694542;
    k_zero: 0.9996;
    e_prime_square: 0.006694542/(1-0.006694542));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  WGS72_1972: TEllipsoid =
    (LowerCase_a: 6378135;
    e_square: 0.006694318;
    k_zero: 0.9996;
    e_prime_square: 0.006694318/(1-0.006694318));

  // @abstract(from Peter H. Dana.)
  // http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html
  // See also http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt
  // @SeeAlso(Ellipsoid)
  WGS84: TEllipsoid =
    (LowerCase_a: 6378137;
    e_square: 0.00669438;
    k_zero: 0.9996;
    e_prime_square: 0.00669438/(1-0.00669438));

var
  {@name is the ellipsoid used for converting coordinates in latitude
  and longitude to UTM coordinates.  See @link(ConvertToUTM)
  @seealso(Airy1830)
  @seealso(Bessel1841)
  @seealso(Clarke1866)
  @seealso(Clarke1880)
  @seealso(Everest1830)
  @seealso(Fischer1960)
  @seealso(Fischer1968)
  @seealso(GRS67_1967)
  @seealso(GRS75_1975)
  @seealso(GRS80_1980)
  @seealso(Hough1956)
  @seealso(International1924)
  @seealso(Krassowsky1940)
  @seealso(SouthAmerican1969)
  @seealso(WGS60_1960)
  @seealso(WGS66_1966)
  @seealso(WGS72_1972)
  @seealso(WGS84)
  }
  Ellipsoid: TEllipsoid;

implementation

uses Math;

function Get_N(LatitudeRadians: double): double;
begin
  result := Ellipsoid.LowerCase_a/Sqrt(1- Ellipsoid.e_square*Sqr(Sin(LatitudeRadians)));
end;

function Get_T(LatitudeRadians: double): double;
begin
  result := Sqr(Tan(LatitudeRadians));
end;

function Get_C(LatitudeRadians: double): double;
begin
  result := Ellipsoid.e_prime_square*Sqr(Cos(LatitudeRadians));
end;

function Get_A(LongitudeRadians, LatitudeRadians,
  CentralMeridianRadians: double): double;
begin
  Result := (LongitudeRadians - CentralMeridianRadians) * Cos(LatitudeRadians);
end;

function Get_M(LatitudeRadians: double): double;
var
  e_fourth, e_sixth : double;
begin
  e_fourth := Sqr(Ellipsoid.e_square);
  e_sixth := Power(Ellipsoid.e_square,3);
  Result := Ellipsoid.LowerCase_a *((1-Ellipsoid.e_square/4 - 3*e_fourth/64 - 5 * e_sixth/256)
    * LatitudeRadians - (3*Ellipsoid.e_square/8 + 3*e_fourth/32 + 45*e_sixth/1024)
    * sin(2*LatitudeRadians) + (15*e_fourth/256 + 45*e_sixth/1024)
    * sin(4*LatitudeRadians) - (35*e_sixth/3072) * sin(6*LatitudeRadians));
end;

procedure ConvertToUTM(const LatitudeRadians,
  LongitudeRadians, CentralMeridianInRadians: double;
  out X, Y: double);
const
  M_zero = 0;
var
  N, A, T, C, M  : double;
begin
  N := Get_N(LatitudeRadians);
  A := Get_A(LongitudeRadians, LatitudeRadians, CentralMeridianInRadians);
  T := Get_T(LatitudeRadians);
  C := Get_C(LatitudeRadians);
  M := Get_M(LatitudeRadians);

  X := Ellipsoid.k_zero * N *(A + (1 - T +C)*Power(A,3)/6 + (5 -18*T + Sqr(T)
    + 72*C - 58*Ellipsoid.e_prime_square)* Power(A,5)/120);

  Y := Ellipsoid.k_zero * (M - M_zero + N * Tan(LatitudeRadians) *
    (Sqr(A)/2 + (5 - T + 9*C + 4*Sqr(C)) * Power(A,4)/24 +
    (61 - 58* T + Sqr(T) + 600 * C - 330*Ellipsoid.e_prime_square)*Power(A,6)/720));

  X := X + 500000;

  if Y < 0 then
  begin
    Y := Y + 10000000
  end;
end;

initialization
  Ellipsoid := Clarke1866;

end.
