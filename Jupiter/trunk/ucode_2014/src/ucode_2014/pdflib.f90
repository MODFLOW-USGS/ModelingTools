MODULE PDFLIB

USE RNGLIB

IMPLICIT NONE

CONTAINS

!*************************************************************************
!***                The following subroutines are from                 ***
!***   http://people.sc.fsu.edu/~jburkardt/f_src/pdflib/pdflib.html    ***
!***                  with minor revisions by Dan Lu                   ***
!*************************************************************************

function i4_binomial_pdf ( n, p, k )

!*****************************************************************************80
!
!! I4_BINOMIAL_PDF evaluates the binomial PDF.
!
!  Discussion:
!
!    pdf(n,p,k) = C(n,k) p^k (1-p)^(n-k)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of binomial trials.
!    0 < N.
!
!    Input, real ( kind = 8 ) P, the probability of a success in one trial.
!
!    Input, integer ( kind = 4 ) K, the number of successes.
!
!    Output, real ( kind = 8 ) I4_BINOMIAL_PDF, the probability of K successes
!    in N trials with a per-trial success probability of P.
!
  implicit none

  real ( kind = 8 ) i4_binomial_pdf
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ) p
!  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) value

  if ( k < 0 ) then
    value = 0.0D+00
  else if ( k <= n ) then
    value = r8_choose ( n, k ) * p ** k * ( 1.0D+00 - p ) ** k
  else
    value = 0.0D+00
  end if

  i4_binomial_pdf = value

  return
end function i4_binomial_pdf

function i4_binomial_sample ( n, pp )

!*****************************************************************************80
!
!! I4_BINOMIAL_SAMPLE generates a binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a binomial
!    distribution whose number of trials is N and whose
!    probability of an event in each trial is P.
!
!    The previous version of this program relied on the assumption that
!    local memory would be preserved between calls.  It set up data
!    one time to be preserved for use over multiple calls.  In the
!    interests of portability, this assumption has been removed, and
!    the "setup" data is recomputed on every call.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Voratas Kachitvichyanukul, Bruce Schmeiser,
!    Binomial Random Variate Generation,
!    Communications of the ACM,
!    Volume 31, Number 2, February 1988, pages 216-222.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of binomial trials, from which a
!    random deviate will be generated.
!    0 < N.
!
!    Input, real ( kind = 8 ) PP, the probability of an event in each trial of
!    the binomial distribution from which a random deviate is to be generated.
!    0.0 < PP < 1.0.
!
!    Output, integer ( kind = 4 ) I4_BINOMIAL_SAMPLE, a random deviate from the
!    distribution.
!
  implicit none

  real ( kind = 8 ) al
  real ( kind = 8 ) alv
  real ( kind = 8 ) amaxp
  real ( kind = 8 ) c
  real ( kind = 8 ) f
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  real ( kind = 8 ) ffm
  real ( kind = 8 ) fm
  real ( kind = 8 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_binomial_sample
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ix1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mp
  real ( kind = 8 ) pp
  integer ( kind = 4 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ) p1
  real ( kind = 8 ) p2
  real ( kind = 8 ) p3
  real ( kind = 8 ) p4
  real ( kind = 8 ) q
  real ( kind = 8 ) qn
  real ( kind = 8 ) r
!  real ( kind = 8 ) r8_uniform_01_sample
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) w2
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) xl
  real ( kind = 8 ) xll
  real ( kind = 8 ) xlr
  real ( kind = 8 ) xm
  real ( kind = 8 ) xnp
  real ( kind = 8 ) xnpq
  real ( kind = 8 ) xr
  real ( kind = 8 ) ynorm
  real ( kind = 8 ) z
  real ( kind = 8 ) z2

  if ( pp <= 0.0D+00 .or. 1.0D+00 <= pp ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_BINOMIAL_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  PP is out of range.'
    stop
  end if

  p = min ( pp, 1.0D+00 - pp )
  q = 1.0D+00 - p
  xnp = real ( n, kind = 8 ) * p

  if ( xnp < 30.0D+00 ) then

    qn = q ** n
    r = p / q
    g = r * real ( n + 1, kind = 8 )

    do

      ix = 0
      f = qn
      u = r8_uniform_01_sample ( )

      do

        if ( u < f ) then
          if ( 0.5D+00 < pp ) then
            ix = n - ix
          end if
          i4_binomial_sample = ix
          return
        end if

        if ( 110 < ix ) then
          exit
        end if

        u = u - f
        ix = ix + 1
        f = f * ( g / real ( ix, kind = 8 ) - r )

      end do

    end do

  end if

  ffm = xnp + p
  m = ffm
  fm = m
  xnpq = xnp * q
  p1 = int ( 2.195D+00 * sqrt ( xnpq ) - 4.6D+00 * q ) + 0.5D+00
  xm = fm + 0.5D+00
  xl = xm - p1
  xr = xm + p1
  c = 0.134D+00 + 20.5D+00 / ( 15.3D+00 + fm )
  al = ( ffm - xl ) / ( ffm - xl * p )
  xll = al * ( 1.0D+00 + 0.5D+00 * al )
  al = ( xr - ffm ) / ( xr * q )
  xlr = al * ( 1.0D+00 + 0.5D+00 * al )
  p2 = p1 * ( 1.0D+00 + c + c )
  p3 = p2 + c / xll
  p4 = p3 + c / xlr
!
!  Generate a variate.
!
  do

    u = r8_uniform_01_sample ( ) * p4
    v = r8_uniform_01_sample ( )
!
!  Triangle
!
    if ( u < p1 ) then
      ix = xm - p1 * v + u
      if ( 0.5D+00 < pp ) then
        ix = n - ix
      end if
      i4_binomial_sample = ix
      return
    end if
!
!  Parallelogram
!
    if ( u <= p2 ) then

      x = xl + ( u - p1 ) / c
      v = v * c + 1.0D+00 - abs ( xm - x ) / p1

      if ( v <= 0.0D+00 .or. 1.0D+00 < v ) then
        cycle
      end if

      ix = x

    else if ( u <= p3 ) then

      ix = xl + log ( v ) / xll
      if ( ix < 0 ) then
        cycle
      end if
      v = v * ( u - p2 ) * xll

    else

      ix = xr - log ( v ) / xlr
      if ( n < ix ) then
        cycle
      end if
      v = v * ( u - p3 ) * xlr

    end if

    k = abs ( ix - m )

    if ( k <= 20 .or. xnpq / 2.0D+00 - 1.0D+00 <= k ) then

      f = 1.0D+00
      r = p / q
      g = real ( n + 1, kind = 8 ) * r

      if ( m < ix ) then
        mp = m + 1
        do i = m + 1, ix
          f = f * ( g / i - r )
        end do
      else if ( ix < m ) then
        ix1 = ix + 1
        do i = ix + 1, m
          f = f / ( g / real ( i, kind = 8 ) - r )
        end do
      end if

      if ( v <= f ) then
        if ( 0.5D+00 < pp ) then
          ix = n - ix
        end if
        i4_binomial_sample = ix
        return
      end if

    else

      amaxp = ( k / xnpq ) * ( ( k * ( k / 3.0D+00 &
        + 0.625D+00 ) + 0.1666666666666D+00 ) / xnpq + 0.5D+00 )
      ynorm = - real ( k * k, kind = 8 ) / ( 2.0D+00 * xnpq )
      alv = log ( v )

      if ( alv < ynorm - amaxp ) then
        if ( 0.5D+00 < pp ) then
          ix = n - ix
        end if
        i4_binomial_sample = ix
        return
      end if

      if ( ynorm + amaxp < alv ) then
        cycle
      end if

      x1 = real ( ix + 1, kind = 8 )
      f1 = fm + 1.0D+00
      z = real ( n + 1, kind = 8 ) - fm
      w = real ( n - ix + 1, kind = 8 )
      z2 = z * z
      x2 = x1 * x1
      f2 = f1 * f1
      w2 = w * w

      t = xm * log ( f1 / x1 ) + ( n - m + 0.5D+00 ) * log ( z / w ) &
        + real ( ix - m, kind = 8 ) * log ( w * p / ( x1 * q )) &
        + ( 13860.0D+00 - ( 462.0D+00 - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 &
        / f2 ) / f2 ) / f2 ) / f2 ) / f1 / 166320.0D+00 &
        + ( 13860.0D+00 - ( 462.0D+00 - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 &
        / z2 ) / z2 ) / z2 ) / z2 ) / z / 166320.0D+00 &
        + ( 13860.0D+00 - ( 462.0D+00 - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 &
        / x2 ) / x2 ) / x2 ) / x2 ) / x1 / 166320.0D+00 &
        + ( 13860.0D+00 - ( 462.0D+00 - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 &
        / w2 ) / w2 ) / w2 ) / w2 ) / w / 166320.0D+00

      if ( alv <= t ) then
        if ( 0.5D+00 < pp ) then
          ix = n - ix
        end if
        i4_binomial_sample = ix
        return
      end if

    end if

  end do

  return
end function i4_binomial_sample

function i4vec_multinomial_pdf ( n, p, m, x )

!*****************************************************************************80
!
!! I4VEC_MULTINOMIAL_PDF evaluates the multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of trials.
!
!    Input, real ( kind = 8 ) P(M), the probability of each outcome
!    on any single trial.
!
!    Input, integer ( kind = 4 ) M, the number of possible outcomes
!    of a single trial.
!
!    Input, integer ( kind = 4 ) X(M), the results of N trials,
!    with X(I) the number of times outcome I occurred.
!
!    Output, real ( kind = 8 ) I4VEC_MULTINOMIAL_PDF, the probability
!    density function evaluated at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) bot
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) i4vec_multinomial_pdf
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(m)
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) top
  integer ( kind = 4 ) x(m)
!
!  The combinatorial coefficient is an integer.
!
  c = 1
  top = n
  do i = 1, m
    bot = 1
    do j = 1, x(i)
      c = ( c * top ) / bot
      top = top - 1
      bot = bot + 1
    end do
  end do

  pdf = real ( c, kind = 8 )
  do i = 1, m
    pdf = pdf * p(i) ** x(i)
  end do

  i4vec_multinomial_pdf = pdf

  return
end function i4vec_multinomial_pdf

subroutine i4vec_multinomial_sample ( n, p, ncat, ix )

!*****************************************************************************80
!
!! I4VEC_MULTINOMIAL_SAMPLE generates a multinomial random deviate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer, 1986,
!    ISBN: 0387963057,
!    LC: QA274.D48.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of trials.
!
!    Input, real ( kind = 8 ) P(NCAT).  P(I) is the probability that an event
!    will be classified into category I.  Thus, each P(I) must be between
!    0.0 and 1.0, and the P's must sum to 1.
!
!    Input, integer ( kind = 4 ) NCAT, the number of possible outcomes
!    of a single trial.
!
!    Output, integer ( kind = 4 ) IX(NCAT), a random observation from
!    the multinomial distribution.  All IX(i) will be nonnegative and their
!    sum will be N.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncat

  integer ( kind = 4 ) i
!  integer ( kind = 4 ) i4_binomial_sample
  integer ( kind = 4 ) icat
  integer ( kind = 4 ) ix(ncat)
  integer ( kind = 4 ) ntot
  real ( kind = 8 ) p(ncat)
  real ( kind = 8 ) prob
  real ( kind = 8 ) ptot

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  N < 0'
    stop
  end if

  if ( ncat <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  NCAT <= 1'
    stop
  end if

  do i = 1, ncat

    if ( p(i) < 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
      write ( *, '(a)' ) '  Some P(i) < 0.'
      stop
    end if

    if ( 1.0D+00 < p(i) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
      write ( *, '(a)' ) '  Some 1 < P(i).'
      stop
    end if

  end do
!
!  Initialize variables.
!
  ntot = n
  ptot = 1.0D+00
  do i = 1, ncat
    ix(i) = 0
  end do
!
!  Generate the observation.
!
  do icat = 1, ncat - 1
    prob = p(icat) / ptot
    ix(icat) = i4_binomial_sample ( ntot, prob )
    ntot = ntot - ix(icat)
    if ( ntot <= 0 ) then
      return
    end if
    ptot = ptot - p(icat)
  end do

  ix(ncat) = ntot

  return
end subroutine i4vec_multinomial_sample

function r8_beta_pdf ( alpha, beta, rval )

!*****************************************************************************80
!
!! R8_BETA_PDF evaluates the PDF of a beta distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, BETA, shape parameters.
!    0.0 < ALPHA, BETA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_BETA_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ) r8_beta_pdf
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) rval
  real ( kind = 8 ) temp

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_BETA_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
    stop
  end if

  if ( beta <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_BETA_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter BETA is not positive.'
    stop
  end if

  if ( rval < 0.0D+00 .or. 1.0D+00 < rval ) then

    r8_beta_pdf = 0.0D+00

  else

    temp = r8_gamma_log ( alpha + beta ) - r8_gamma_log ( alpha ) &
      - r8_gamma_log ( beta )

    r8_beta_pdf = exp ( temp ) * rval ** ( alpha - 1.0D+00 ) &
      * ( 1.0D+00 - rval ) ** ( beta - 1.0D+00 )

  end if

  return
end function r8_beta_pdf

function r8_beta_sample ( aa, bb )

!*****************************************************************************80
!
!! R8_BETA_SAMPLE generates a beta random deviate.
!
!  Discussion:
!
!    This procedure returns a single random deviate from the beta distribution
!    with parameters A and B.  The density is
!
!      x^(a-1) * (1-x)^(b-1) / Beta(a,b) for 0 < x < 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Russell Cheng,
!    Generating Beta Variates with Nonintegral Shape Parameters,
!    Communications of the ACM,
!    Volume 21, Number 4, April 1978, pages 317-322.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AA, the first parameter of the beta distribution.
!    0.0 < AA.
!
!    Input, real ( kind = 8 ) BB, the second parameter of the beta distribution.
!    0.0 < BB.
!
!    Output, real ( kind = 8 ) R8_BETA_SAMPLE, a beta random variate.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) aa
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) bb
  real ( kind = 8 ) beta
  real ( kind = 8 ) delta
  real ( kind = 8 ) gamma
  real ( kind = 8 ) k1
  real ( kind = 8 ) k2
  real ( kind = 8 ), parameter :: log4 = 1.3862943611198906188D+00
  real ( kind = 8 ), parameter :: log5 = 1.6094379124341003746D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_beta_sample
!  real ( kind = 8 ) r8_uniform_01_sample
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) u1
  real ( kind = 8 ) u2
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  if ( aa <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_BETA_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  AA <= 0.0'
    stop
  end if

  if ( bb <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_BETA_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  BB <= 0.0'
    stop
  end if
!
!  Algorithm BB
!
  if ( 1.0D+00 < aa .and. 1.0D+00 < bb ) then

    a = min ( aa, bb )
    b = max ( aa, bb )
    alpha = a + b
    beta = sqrt ( ( alpha - 2.0D+00 ) / ( 2.0D+00 * a * b - alpha ) )
    gamma = a + 1.0D+00 / beta

    do

      u1 = r8_uniform_01_sample ( )
      u2 = r8_uniform_01_sample ( )
      v = beta * log ( u1 / ( 1.0D+00 - u1 ) )
      w = a * exp ( v )

      z = u1 ** 2 * u2
      r = gamma * v - log4
      s = a + r - w

      if ( 5.0D+00 * z <= s + 1.0D+00 + log5 ) then
        exit
      end if

      t = log ( z )
      if ( t <= s ) then
        exit
      end if

      if ( t <= ( r + alpha * log ( alpha / ( b + w ) ) ) ) then
        exit
      end if

    end do
!
!  Algorithm BC
!
  else

    a = max ( aa, bb )
    b = min ( aa, bb )
    alpha = a + b
    beta = 1.0D+00 / b
    delta = 1.0D+00 + a - b
    k1 = delta * ( 1.0D+00 / 72.0D+00 + b / 24.0D+00 ) &
      / ( a / b - 7.0D+00 / 9.0D+00 )
    k2 = 0.25D+00 + ( 0.5D+00 + 0.25D+00 / delta ) * b

    do

      u1 = r8_uniform_01_sample ( )
      u2 = r8_uniform_01_sample ( )

      if ( u1 < 0.5D+00 ) then

        y = u1 * u2
        z = u1 * y

        if ( k1 <= 0.25D+00 * u2 + z - y ) then
          cycle
        end if

      else

        z = u1 ** 2 * u2

        if ( z <= 0.25D+00 ) then

          v = beta * log ( u1 / ( 1.0D+00 - u1 ) )
          w = a * exp ( v )

          if ( aa == a ) then
            r8_beta_sample = w / ( b + w )
          else
            r8_beta_sample = b / ( b + w )
          end if

          return

        end if

        if ( k2 < z ) then
          cycle
        end if

      end if

      v = beta * log ( u1 / ( 1.0D+00 - u1 ) )
      w = a * exp ( v )

      if ( log ( z ) <= alpha * ( log ( alpha / ( b + w ) ) + v ) - log4 ) then
        exit
      end if

    end do

  end if

  if ( aa == a ) then
    r8_beta_sample = w / ( b + w )
  else
    r8_beta_sample = b / ( b + w )
  end if

  return
end function r8_beta_sample

function r8_chi_pdf ( df, rval )

!*****************************************************************************80
!
!! R8_CHI_PDF evaluates the PDF of a chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_CHI_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) df
  real ( kind = 8 ) r8_chi_pdf
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) rval
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2

  if ( df <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CHI_PDF - Fatal error!'
    write ( *, '(a)' ) '  Degrees of freedom must be positive.'
    stop
  end if

  if ( rval <= 0.0D+00 ) then

    r8_chi_pdf = 0.0D+00

  else

    temp2 = df * 0.5D+00

    temp1 = ( temp2 - 1.0D+00 ) * log ( rval ) - 0.5D+00 * rval &
      - temp2 * log ( 2.0D+00 ) - r8_gamma_log ( temp2 )

    r8_chi_pdf = exp ( temp1 )

  end if

  return
end function r8_chi_pdf

function r8_chi_sample ( df )

!*****************************************************************************80
!
!! R8_CHI_SAMPLE generates a Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the chi square distribution
!    with DF degrees of freedom random variable.
!
!    The algorithm exploits the relation between chisquare and gamma.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 8 ) R8_CHI_SAMPLE, a random deviate
!    from the distribution.
!
  implicit none

  real ( kind = 8 ) arg1
  real ( kind = 8 ) arg2
  real ( kind = 8 ) df
  real ( kind = 8 ) r8_chi_sample
!  real ( kind = 8 ) r8_gamma_sample

  if ( df <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CHI_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  DF <= 0.'
    write ( *, '(a,g14.6)' ) '  Value of DF: ', df
    stop
  end if

  arg1 = 1.0D+00
  arg2 = df / 2.0D+00

  r8_chi_sample = 2.0D+00 * r8_gamma_sample ( arg1, arg2 )

  return
end function r8_chi_sample

function r8_choose ( n, k )

!*****************************************************************************80
!
!! R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in R8 arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, real ( kind = 8 ) R8_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) mx
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) value

  mn = min ( k, n - k )

  if ( mn < 0 ) then

    value = 0.0D+00

  else if ( mn == 0 ) then

    value = 1.0D+00

  else

    mx = max ( k, n - k )
    value = real ( mx + 1, kind = 8 )

    do i = 2, mn
      value = ( value * real ( mx + i, kind = 8 ) ) / real ( i, kind = 8 )
    end do

  end if

  r8_choose = value

  return
end function r8_choose

function r8_exponential_pdf ( beta, rval )

!*****************************************************************************80
!
!! R8_EXPONENTIAL_PDF evaluates the PDF of an exponential distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the scale value.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) beta
  real ( kind = 8 ) r8_exponential_pdf
  real ( kind = 8 ) rval

  if ( beta <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_EXPONENTIAL_PDF - Fatal error!'
    write ( *, '(a)' ) '  BETA parameter must be positive.'
    stop
  end if

  if ( rval < 0.0D+00 ) then
    r8_exponential_pdf = 0.0D+00
  else
    r8_exponential_pdf = exp ( - rval / beta ) / beta
  end if

  return
end function r8_exponential_pdf

function r8_exponential_sample ( lambda )

!*****************************************************************************80
!
!! R8_EXPONENTIAL_SAMPLE samples the exponential PDF.
!
!  Discussion:
!
!    Note that the parameter LAMBDA is a multiplier.  In some formulations,
!    it is used as a divisor instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) lambda
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_exponential_sample
!  real ( kind = 8 ) r8_uniform_01_sample

  if ( lambda <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_EXPONENTIAL_SAMPLE - Fatal error!'
    write ( *, '(a)' ) 'LAMBDA parameter must be positive.'
    stop
  end if

  r = r8_uniform_01_sample ( )

  r8_exponential_sample = - log ( r ) * lambda

  return
end function r8_exponential_sample

function r8_exponential_01_pdf ( rval )

!*****************************************************************************80
!
!! R8_EXPONENTIAL_01_PDF: PDF of a standard exponential distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_01_PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) r8_exponential_01_pdf
  real ( kind = 8 ) rval

  if ( rval < 0.0D+00 ) then
    r8_exponential_01_pdf = 0.0D+00
  else
    r8_exponential_01_pdf = exp ( - rval )
  end if

  return
end function r8_exponential_01_pdf

function r8_exponential_01_sample ( )

!*****************************************************************************80
!
!! R8_EXPONENTIAL_01_SAMPLE samples the standard exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_01_SAMPLE, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) r
  real ( kind = 8 ) r8_exponential_01_sample
!  real ( kind = 8 ) r8_uniform_01_sample

  r = r8_uniform_01_sample ( )

  r8_exponential_01_sample = - log ( r )

  return
end function r8_exponential_01_sample

function r8_gamma_log ( x )

!*****************************************************************************80
!
!! R8_GAMMA_LOG evaluates the logarithm of the gamma function.
!
!  Discussion:
!
!    This routine calculates the LOG(GAMMA) function for a positive real
!    argument X.  Computation is based on an algorithm outlined in
!    references 1 and 2.  The program uses rational functions that
!    theoretically approximate LOG(GAMMA) to at least 18 significant
!    decimal digits.  The approximation for X > 12 is from reference
!    3, while approximations for X < 12.0 are similar to those in
!    reference 1, but are unpublished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the
!    Gamma Function,
!    Mathematics of Computation,
!    Volume 21, Number 98, April 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG, the value of the function.
!
  implicit none

  real ( kind = 8 ), dimension ( 7 ) :: c = (/ &
    -1.910444077728D-03, &
     8.4171387781295D-04, &
    -5.952379913043012D-04, &
     7.93650793500350248D-04, &
    -2.777777777777681622553D-03, &
     8.333333333333333331554247D-02, &
     5.7083835261D-03 /)
  real ( kind = 8 ) corr
  real ( kind = 8 ) :: d1 = -5.772156649015328605195174D-01
  real ( kind = 8 ) :: d2 = 4.227843350984671393993777D-01
  real ( kind = 8 ) :: d4 = 1.791759469228055000094023D+00
  real ( kind = 8 ), parameter :: frtbig = 2.25D+76
  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension ( 8 ) :: p1 = (/ &
    4.945235359296727046734888D+00, &
    2.018112620856775083915565D+02, &
    2.290838373831346393026739D+03, &
    1.131967205903380828685045D+04, &
    2.855724635671635335736389D+04, &
    3.848496228443793359990269D+04, &
    2.637748787624195437963534D+04, &
    7.225813979700288197698961D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: p2 = (/ &
    4.974607845568932035012064D+00, &
    5.424138599891070494101986D+02, &
    1.550693864978364947665077D+04, &
    1.847932904445632425417223D+05, &
    1.088204769468828767498470D+06, &
    3.338152967987029735917223D+06, &
    5.106661678927352456275255D+06, &
    3.074109054850539556250927D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: p4 = (/ &
    1.474502166059939948905062D+04, &
    2.426813369486704502836312D+06, &
    1.214755574045093227939592D+08, &
    2.663432449630976949898078D+09, &
    2.940378956634553899906876D+10, &
    1.702665737765398868392998D+11, &
    4.926125793377430887588120D+11, &
    5.606251856223951465078242D+11 /)
  real ( kind = 8 ), dimension ( 8 ) :: q1 = (/ &
    6.748212550303777196073036D+01, &
    1.113332393857199323513008D+03, &
    7.738757056935398733233834D+03, &
    2.763987074403340708898585D+04, &
    5.499310206226157329794414D+04, &
    6.161122180066002127833352D+04, &
    3.635127591501940507276287D+04, &
    8.785536302431013170870835D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: q2 = (/ &
    1.830328399370592604055942D+02, &
    7.765049321445005871323047D+03, &
    1.331903827966074194402448D+05, &
    1.136705821321969608938755D+06, &
    5.267964117437946917577538D+06, &
    1.346701454311101692290052D+07, &
    1.782736530353274213975932D+07, &
    9.533095591844353613395747D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: q4 = (/ &
    2.690530175870899333379843D+03, &
    6.393885654300092398984238D+05, &
    4.135599930241388052042842D+07, &
    1.120872109616147941376570D+09, &
    1.488613728678813811542398D+10, &
    1.016803586272438228077304D+11, &
    3.417476345507377132798597D+11, &
    4.463158187419713286462081D+11 /)
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) res
  real ( kind = 8 ), parameter :: sqrtpi = 0.9189385332046727417803297D+00
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 2.55D+305
  real ( kind = 8 ) xden
  real ( kind = 8 ), parameter :: xinf = 1.79D+308
  real ( kind = 8 ) xm1
  real ( kind = 8 ) xm2
  real ( kind = 8 ) xm4
  real ( kind = 8 ) xnum
  real ( kind = 8 ) y
  real ( kind = 8 ) ysq

  y = x

  if ( 0.0D+00 < y .and. y <= xbig ) then

    if ( y <= epsilon ( y ) ) then

      res = - log ( y )
!
!  EPS < X <= 1.5.
!
    else if ( y <= 1.5D+00 ) then

      if ( y < 0.6796875D+00 ) then
        corr = -log ( y )
        xm1 = y
      else
        corr = 0.0D+00
        xm1 = ( y - 0.5D+00 ) - 0.5D+00
      end if

      if ( y <= 0.5D+00 .or. 0.6796875D+00 <= y ) then

        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm1 + p1(i)
          xden = xden * xm1 + q1(i)
        end do

        res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

      else

        xm2 = ( y - 0.5D+00 ) - 0.5D+00
        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm2 + p2(i)
          xden = xden * xm2 + q2(i)
        end do

        res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

      end if
!
!  1.5 < X <= 4.0.
!
    else if ( y <= 4.0D+00 ) then

      xm2 = y - 2.0D+00
      xden = 1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm2 + p2(i)
        xden = xden * xm2 + q2(i)
      end do

      res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
!
!  4.0 < X <= 12.0.
!
    else if ( y <= 12.0D+00 ) then

      xm4 = y - 4.0D+00
      xden = -1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm4 + p4(i)
        xden = xden * xm4 + q4(i)
      end do

      res = d4 + xm4 * ( xnum / xden )
!
!  Evaluate for 12 <= argument.
!
    else

      res = 0.0D+00

      if ( y <= frtbig ) then

        res = c(7)
        ysq = y * y

        do i = 1, 6
          res = res / ysq + c(i)
        end do

      end if

      res = res / y
      corr = log ( y )
      res = res + sqrtpi - 0.5D+00 * corr
      res = res + y * ( corr - 1.0D+00 )

    end if
!
!  Return for bad arguments.
!
  else

    res = xinf

  end if
!
!  Final adjustments and return.
!
  r8_gamma_log = res

  return
end function r8_gamma_log

function r8_gamma_pdf ( beta, alpha, rval )

!*****************************************************************************80
!
!! R8_GAMMA_PDF evaluates the PDF of a gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the rate parameter.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_GAMMA_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) r8_gamma_pdf
  real ( kind = 8 ) rval
  real ( kind = 8 ) temp

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
    stop
  end if

  if ( beta <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter BETA is not positive.'
    stop
  end if

  if ( rval <= 0.0D+00 ) then

    r8_gamma_pdf = 0.0D+00

  else

    temp = alpha * log ( beta ) + ( alpha - 1.0D+00 ) * log ( rval ) &
      - beta * rval - r8_gamma_log ( alpha )

    r8_gamma_pdf = exp ( temp )

  end if

  return
end function r8_gamma_pdf

function r8_gamma_sample ( a, r )

!*****************************************************************************80
!
!! R8_GAMMA_SAMPLE generates a Gamma random deviate.
!
!  Discussion:
!
!    This procedure generates random deviates from the gamma distribution whose
!    density is (A^R)/Gamma(R) * X^(R-1) * Exp(-A*X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling from Gamma, Beta, Poisson and
!    Binomial Distributions,
!    Computing,
!    Volume 12, Number 3, September 1974, pages 223-246.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the rate parameter.
!
!    Input, real ( kind = 8 ) R, the shape parameter.
!
!    Output, real ( kind = 8 ) R8_GAMMA_SAMPLE, a random deviate
!    from the distribution.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_gamma_sample
!  real ( kind = 8 ) r8_gamma_01_sample

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
    stop
  end if

  if ( r <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Parameter BETA is not positive.'
    stop
  end if

  r8_gamma_sample = r8_gamma_01_sample ( r ) / a

  return
end function r8_gamma_sample

function r8_gamma_01_pdf ( alpha, rval )

!*****************************************************************************80
!
!! R8_GAMMA_01_PDF evaluates the PDF of a standard gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_GAMMA_01_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) alpha
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) r8_gamma_01_pdf
  real ( kind = 8 ) rval
  real ( kind = 8 ) temp

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_01_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
    stop
  end if

  if ( rval <= 0.0D+00 ) then

    r8_gamma_01_pdf = 0.0D+00

  else

    temp = ( alpha - 1.0D+00 ) * log ( rval ) - rval - r8_gamma_log ( alpha )

    r8_gamma_01_pdf = exp ( temp )

  end if

  return
end function r8_gamma_01_pdf

function r8_gamma_01_sample ( a )

!*****************************************************************************80
!
!! R8_GAMMA_01_SAMPLE samples the standard Gamma distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm GD in the reference.
!
!    pdf ( a; x ) = 1/gamma(a) * x^(a-1) * exp ( - x )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the shape parameter of the standard gamma
!    distribution. 0 < A.
!
!    Output, real ( kind = 8 ) R8_GAMMA_01_SAMPLE, a random deviate
!    from the distribution.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: a1 =  0.3333333D+00
  real ( kind = 8 ), parameter :: a2 = -0.2500030D+00
  real ( kind = 8 ), parameter :: a3 =  0.2000062D+00
  real ( kind = 8 ), parameter :: a4 = -0.1662921D+00
  real ( kind = 8 ), parameter :: a5 =  0.1423657D+00
  real ( kind = 8 ), parameter :: a6 = -0.1367177D+00
  real ( kind = 8 ), parameter :: a7 =  0.1233795D+00
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ), parameter :: e1 = 1.0D+00
  real ( kind = 8 ), parameter :: e2 = 0.4999897D+00
  real ( kind = 8 ), parameter :: e3 = 0.1668290D+00
  real ( kind = 8 ), parameter :: e4 = 0.0407753D+00
  real ( kind = 8 ), parameter :: e5 = 0.0102930D+00
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) q0
  real ( kind = 8 ), parameter :: q1 =  0.04166669D+00
  real ( kind = 8 ), parameter :: q2 =  0.02083148D+00
  real ( kind = 8 ), parameter :: q3 =  0.00801191D+00
  real ( kind = 8 ), parameter :: q4 =  0.00144121D+00
  real ( kind = 8 ), parameter :: q5 = -0.00007388D+00
  real ( kind = 8 ), parameter :: q6 =  0.00024511D+00
  real ( kind = 8 ), parameter :: q7 =  0.00024240D+00
  real ( kind = 8 ) r
!  real ( kind = 8 ) r8_exponential_01_sample
  real ( kind = 8 ) r8_gamma_01_sample
!  real ( kind = 8 ) r8_normal_01_sample
!  real ( kind = 8 ) r8_uniform_01_sample
  real ( kind = 8 ) s
  real ( kind = 8 ) s2
  real ( kind = 8 ) si
  real ( kind = 8 ), parameter :: sqrt32 = 5.656854D+00
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( 1.0D+00 <= a ) then

    s2 = a - 0.5D+00
    s = sqrt ( s2 )
    d = sqrt32 - 12.0D+00 * s
!
!  Immediate acceptance.
!
    t = r8_normal_01_sample ( )
    x = s + 0.5D+00 * t
    r8_gamma_01_sample = x * x

    if ( 0.0D+00 <= t ) then
      return
    end if
!
!  Squeeze acceptance.
!
    u = r8_uniform_01_sample ( )
    if ( d * u <= t * t * t ) then
      return
    end if

    r = 1.0D+00 / a
    q0 = (((((( q7 &
      * r + q6 ) &
      * r + q5 ) &
      * r + q4 ) &
      * r + q3 ) &
      * r + q2 ) &
      * r + q1 ) &
      * r
!
!  Approximation depending on size of parameter A.
!
    if ( 13.022D+00 < a ) then
      b = 1.77D+00
      si = 0.75D+00
      c = 0.1515D+00 / s
    else if ( 3.686D+00 < a ) then
      b = 1.654D+00 + 0.0076D+00 * s2
      si = 1.68D+00 / s + 0.275D+00
      c = 0.062D+00 / s + 0.024D+00
    else
      b = 0.463D+00 + s + 0.178D+00 * s2
      si = 1.235D+00
      c = 0.195D+00 / s - 0.079D+00 + 0.16D+00 * s
    end if
!
!  Quotient test.
!
    if ( 0.0D+00 < x ) then

      v = 0.5D+00 * t / s

      if ( 0.25D+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25D+00 * t * t + 2.0D+00 * s2 * log ( 1.0D+00 + v )
      else
        q = q0 + 0.5D+00 * t * t * (((((( a7 &
          * v + a6 ) &
          * v + a5 ) &
          * v + a4 ) &
          * v + a3 ) &
          * v + a2 ) &
          * v + a1 ) &
          * v
      end if

      if ( log ( 1.0D+00 - u ) <= q ) then
        return
      end if

    end if

    do

      e = r8_exponential_01_sample ( )
      u = 2.0D+00 * r8_uniform_01_sample ( ) - 1.0D+00

      if ( 0.0D+00 <= u ) then
        t = b + abs ( si * e )
      else
        t = b - abs ( si * e )
      end if
!
!  Possible rejection.
!
      if ( t < -0.7187449D+00 ) then
        cycle
      end if
!
!  Calculate V and quotient Q.
!
      v = 0.5D+00 * t / s

      if ( 0.25D+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25D+00 * t * t + 2.0D+00 * s2 * log ( 1.0D+00 + v )
      else
        q = q0 + 0.5D+00 * t * t * (((((( a7 &
          * v + a6 ) &
          * v + a5 ) &
          * v + a4 ) &
          * v + a3 ) &
          * v + a2 ) &
          * v + a1 ) &
          *  v
      end if
!
!  Hat acceptance.
!
      if ( q <= 0.0D+00 ) then
        cycle
      end if

      if ( 0.5D+00 < q ) then
        w = exp ( q ) - 1.0D+00
      else
        w = (((( e5 * q + e4 ) * q + e3 ) * q + e2 ) * q + e1 ) * q
      end if
!
!  May have to sample again.
!
      if ( c * abs ( u ) <= w * exp ( e - 0.5D+00 * t * t ) ) then
        exit
      end if

    end do

    x = s + 0.5D+00 * t
    r8_gamma_01_sample = x * x

    return
!
!  Method for A < 1.
!
  else

    b = 1.0D+00 + 0.3678794D+00 * a

    do

      p = b * r8_uniform_01_sample ( )

      if ( p < 1.0D+00 ) then

        r8_gamma_01_sample = exp ( log ( p ) / a )

        if ( r8_gamma_01_sample <= r8_exponential_01_sample ( ) ) then
          return
        end if

        cycle

      end if

      r8_gamma_01_sample = - log ( ( b - p ) / a )

      if ( ( 1.0D+00 - a ) * log ( r8_gamma_01_sample ) <= &
        r8_exponential_01_sample ( ) ) then
        exit
      end if

    end do

  end if

  return
end function r8_gamma_01_sample

function r8_invchi_pdf ( df, rval )

!*****************************************************************************80
!
!! R8_INVCHI_PDF evaluates the PDF of an inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_INVCHI_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) df
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) r8_invchi_pdf
  real ( kind = 8 ) rval
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2

  if ( df <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_INVCHI_PDF - Fatal error!'
    write ( *, '(a)' ) '  Degrees of freedom must be positive.'
    stop
  end if

  if ( rval <= 0.0D+00 ) then

    r8_invchi_pdf = 0.0D+00

  else

    temp2 = df * 0.5D+00
    temp1 = - temp2 * log ( 2.0D+00 ) - ( temp2 + 1.0D+00 ) * log ( rval ) &
      - 0.5D+00 / rval - r8_gamma_log ( temp2 )

    r8_invchi_pdf = exp ( temp1 )

  end if

  return
end function r8_invchi_pdf

function r8_invchi_sample ( df )

!*****************************************************************************80
!
!! R8_INVCHI_SAMPLE samples an inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 8 ) R8_INVCHI_SAMPLE, a sample value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) df
!  real ( kind = 8 ) r8_gamma_sample
  real ( kind = 8 ) r8_invchi_sample
  real ( kind = 8 ) value

  if ( df <= 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_INVCHI_SAMPLE - Fatal error!'
      write ( *, '(a)' ) '  Degrees of freedom must be positive.'
      stop
  end if

  a = 0.5D+00
  b = 0.5D+00 * df

  value = r8_gamma_sample ( a, b )

  if ( value /= 0.0D+00 ) then
    value = 1.0D+00 / value
  end if

  r8_invchi_sample = value

  return
end function r8_invchi_sample

function r8_invgam_pdf ( beta, alpha, rval )

!*****************************************************************************80
!
!! R8_INVGAM_PDF evaluates the PDF of an inverse gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the rate parameter.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_INVGAM_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) r8_invgam_pdf
  real ( kind = 8 ) rval
  real ( kind = 8 ) temp

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_INVGAM_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
    stop
  end if

  if ( beta <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_INVGAM_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter BETA is not positive.'
    stop
  end if

  if ( rval <= 0.0D+00 ) then

    r8_invgam_pdf = 0.0D+00

  else

    temp = alpha * log ( beta ) - ( alpha + 1.0D+00 ) * log ( rval ) &
      - beta / rval - r8_gamma_log ( alpha )

    r8_invgam_pdf = exp ( temp )

  end if

  return
end function r8_invgam_pdf

function r8_invgam_sample ( beta, alpha )

!*****************************************************************************80
!
!! R8_INVGAM_SAMPLE samples an inverse gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the rate parameter.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Output, real ( kind = 8 ) R8_INVGAM_SAMPLE, a sample value.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
!  real ( kind = 8 ) r8_gamma_sample
  real ( kind = 8 ) r8_invgam_sample
  real ( kind = 8 ) value

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_INVGAM_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
    stop
  end if

  if ( beta <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_INVGAM_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Parameter BETA is not positive.'
    stop
  end if

  value = r8_gamma_sample ( beta, alpha )

  if ( value /= 0.0D+00 ) then
    value = 1.0D+00 / value
  end if

  r8_invgam_sample = value

  return
end function r8_invgam_sample

function r8_normal_pdf ( av, sd, rval )

!*****************************************************************************80
!
!! R8_NORMAL_PDF evaluates the PDF of a normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AV, the mean value.
!
!    Input, real ( kind = 8 ) SD, the standard deviation.
!    0.0 < SD.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_NORMAL_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) av
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_normal_pdf
  real ( kind = 8 ) rtemp
  real ( kind = 8 ) rval
  real ( kind = 8 ) sd

  if ( sd <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_NORMAL_PDF - Fatal error!'
    write ( *, '(a)' ) '  Standard deviation must be positive.'
    stop
  end if

  rtemp = ( rval - av ) * ( rval - av ) * 0.5D+00 / ( sd * sd )

  r8_normal_pdf = exp ( - rtemp ) / sd / sqrt ( 2.0D+00 * pi )

  return
end function r8_normal_pdf


function r8_lognormal_pdf ( av, sd, rval )

!*****************************************************************************80
!
!! R8_LOGNORMAL_PDF evaluates the PDF of a normal distribution.
!
!Added by Dan Lu
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AV, the mean value.
!
!    Input, real ( kind = 8 ) SD, the standard deviation.
!    0.0 < SD.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_LOGNORMAL_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) av
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_lognormal_pdf
  real ( kind = 8 ) rtemp
  real ( kind = 8 ) rval
  real ( kind = 8 ) sd

  if ( sd <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_LOGNORMAL_PDF - Fatal error!'
    write ( *, '(a)' ) '  Standard deviation must be positive.'
    stop
  end if

  rtemp = ( log(rval) - av ) * ( log(rval) - av ) * 0.5D+00 / ( sd * sd )

  r8_lognormal_pdf = exp ( - rtemp ) /rval/sd/sqrt ( 2.0D+00 * pi )

  return
end function r8_lognormal_pdf



function r8_normal_sample ( av, sd )

!*****************************************************************************80
!
!! R8_NORMAL_SAMPLE generates a normal random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a normal distribution
!    with mean AV, and standard deviation SD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AV, the mean.
!
!    Input, real ( kind = 8 ) SD, the standard deviation.
!
!    Output, real ( kind = 8 ) R8_NORMAL_SAMPLE, a random deviate
!    from the distribution.
!
  implicit none

  real ( kind = 8 ) av
  real ( kind = 8 ) r8_normal_sample
!  real ( kind = 8 ) r8_normal_01_sample
  real ( kind = 8 ) sd

  if ( sd <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_NORMAL_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Standard deviation must be positive.'
    stop
  end if

  r8_normal_sample = sd * r8_normal_01_sample ( ) + av

  return
end function r8_normal_sample

function r8_normal_01_pdf ( rval )

!*****************************************************************************80
!
!! R8_NORMAL_01_PDF evaluates the PDF of a standard normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_NORMAL_01_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_normal_01_pdf
  real ( kind = 8 ) rval

  r8_normal_01_pdf = exp ( - 0.5D+00 * rval ** 2 ) / sqrt ( 2.0D+00 * pi )

  return
end function r8_normal_01_pdf

function r8_normal_01_sample ( )

!*****************************************************************************80
!
!! R8_NORMAL_01_SAMPLE returns a unit pseudonormal R8.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!
!    Typically, we would use one value and save the other for the next call.
!    However, the fact that this function has saved memory makes it difficult
!    to correctly handle cases where we want to re-initialize the code,
!    or to run in parallel.  Therefore, we will instead use the first value
!    and DISCARD the second.
!
!    EFFICIENCY must defer to SIMPLICITY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_NORMAL_01_SAMPLE, a sample of the standard
!    normal PDF.
!
  implicit none

  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_normal_01_sample
!  real ( kind = 8 ) r8_uniform_01_sample
  real ( kind = 8 ) x

  r1 = r8_uniform_01_sample ( )
  r2 = r8_uniform_01_sample ( )

  x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )

  r8_normal_01_sample = x

  return
end function r8_normal_01_sample

function r8_scinvchi_pdf ( df, s, rval )

!*****************************************************************************80
!
!! R8_SCINVCHI_PDF: PDF for a scaled inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) S, the scale factor.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_SCINVCHI_PDF, the value of the PDF at RVAL.
!    inverse-chi-square distribution.
!
  implicit none

  real ( kind = 8 ) df
!  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) r8_scinvchi_pdf
  real ( kind = 8 ) rval
  real ( kind = 8 ) s
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2

  if ( df <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_SCINVCHI_PDF - Fatal error!'
    write ( *, '(a)' ) '  Degrees of freedom must be positive.'
    stop
  end if

  if ( s <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_SCINVCHI_PDF - Fatal error!'
    write ( *, '(a)' ) '  Scale parameter must be positive.'
    stop
  end if

  if ( rval <= 0.0D+00 ) then

    r8_scinvchi_pdf = 0.0D+00

  else

    temp2 = df * 0.5D+00
    temp1 = temp2 * log ( temp2 ) + temp2 * log ( s ) &
      - ( temp2 * s / rval ) &
      - ( temp2 + 1.0D+00 ) * log ( rval ) - r8_gamma_log ( temp2 )

    r8_scinvchi_pdf = exp ( temp1 )

  end if

  return
end function r8_scinvchi_pdf

function r8_scinvchi_sample ( df, s )

!*****************************************************************************80
!
!! R8_SCINVCHI_SAMPLE: sample a scaled inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) S, the scale factor.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) R8_SCINVCHI_SAMPLE, a sample value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) df
!  real ( kind = 8 ) r8_gamma_sample
  real ( kind = 8 ) r8_scinvchi_sample
  real ( kind = 8 ) s
  real ( kind = 8 ) value

  if ( df <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_SCINVCHI_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Degrees of freedom must be positive.'
    stop
  end if

  if ( s <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_SCINVCHI_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Scale parameter must be positive.'
    stop
  end if

  a = 0.5D+00 * df * s
  b = 0.5D+00 * df

  value = r8_gamma_sample ( a, b )

  if ( value /= 0.0D+00 ) then
        value = 1.0D+00 / value
  end if

  r8_scinvchi_sample = value

  return
end function r8_scinvchi_sample

function r8_uniform_pdf ( lower, upper, rval )

!*****************************************************************************80
!
!! R8_UNIFORM_PDF evaluates the PDF of a uniform distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LOWER, UPPER, the lower and upper range limits.
!    LOWER < UPPER.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) lower
  real ( kind = 8 ) r8_uniform_pdf
  real ( kind = 8 ) rval
  real ( kind = 8 ) upper

  if ( upper <= lower ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_PDF - Fatal error!'
    write ( *, '(a)' ) '  For uniform PDF, the lower limit must be '
    write ( *, '(a)' ) '  less than the upper limit!'
    stop
  end if

  if ( rval < lower ) then
    r8_uniform_pdf = 0.0D+00
  else if ( rval <= upper ) then
    r8_uniform_pdf = 1.0D+00 / ( upper - lower )
  else
    r8_uniform_pdf = 0.0D+00
  end if

  return
end function r8_uniform_pdf

function r8_uniform_sample ( low, high )

!*****************************************************************************80
!
!! R8_UNIFORM_SAMPLE generates a uniform random deviate.
!
!  Discussion:
!
!    This procedure generates a real deviate uniformly distributed between
!    LOW and HIGH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_SAMPLE, a random deviate
!    from the distribution.
!
  implicit none

  real ( kind = 8 ) high
  real ( kind = 8 ) low
!  real ( kind = 8 ) r8_uniform_01_sample
  real ( kind = 8 ) r8_uniform_sample

  if ( high <= low ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  For uniform PDF, the lower limit must be '
    write ( *, '(a)' ) '  less than the upper limit!'
    stop
  end if

  r8_uniform_sample = low + ( high - low ) * r8_uniform_01_sample ( )

  return
end function r8_uniform_sample

function r8_uniform_01_pdf ( rval )

!*****************************************************************************80
!
!! R8_UNIFORM_01_PDF evaluates the PDF of a standard uniform distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01_PDF, the value of the PDF at RVAL.
!
  implicit none

  real ( kind = 8 ) r8_uniform_01_pdf
  real ( kind = 8 ) rval

  if ( rval < 0.0D+00 ) then
    r8_uniform_01_pdf = 0.0D+00
  else if ( rval <= 1.0D+00 ) then
    r8_uniform_01_pdf = 1.0D+00
  else
    r8_uniform_01_pdf = 0.0D+00
  end if

  return
end function r8_uniform_01_pdf

function r8_uniform_01_sample ( )

!*****************************************************************************80
!
!! R8_UNIFORM_01_SAMPLE generates a uniform random deviate from [0,1].
!
!  Discussion:
!
!    This function should be the only way that the package accesses random
!    numbers.
!
!    Setting OPTION to 0 accesses the R8_UNI_01() function in RNGLIB,
!    for which there are versions in various languages, which should result
!    in the same values being returned.
!
!    Setting OPTION to 1 in the FORTRAN90 version calls the system
!    RNG "random_number()".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01_SAMPLE, a random deviate
!    from the distribution.
!
  implicit none

  integer ( kind = 4 ), parameter :: option = 0
!  real ( kind = 8 ) r8_uni_01
  real ( kind = 8 ) r8_uniform_01_sample
  real ( kind = 8 ) value

  if ( option == 0 ) then
    value = r8_uni_01 ( )
  else
    call random_number ( harvest = value )
  end if

  r8_uniform_01_sample = value

  return
end function r8_uniform_01_sample

subroutine r8mat_podet ( n, r, det )

!*****************************************************************************80
!
!! R8MAT_PODET computes the determinant of a factored positive definite matrix.
!
!  Discussion:
!
!    This routine expects to receive R, the upper triangular factor of A,
!    computed by R8MAT_POFAC, with the property that A = R' * R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) R(N,N), the Cholesky factor of A.
!
!    Output, real ( kind = 8 ) DET, the determinant of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n,n)

  det = 1.0D+00
  do i = 1, n
    det = det * r(i,i) * r(i,i)
  end do

  return
end subroutine r8mat_podet

subroutine r8mat_pofac ( n, a, r )

!*****************************************************************************80
!
!! R8MAT_POFAC factors a real symmetric positive definite matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the symmetric matrix
!    to be factored.  Only the diagonal and upper triangle are used.
!
!    Output, real ( kind = 8 ) R(N,N), an upper triangular matrix such that
!    A = R'*R.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) s
  real ( kind = 8 ) t

  do j = 1, n
    do i = 1, j
      r(i,j) = a(i,j)
    end do
    do i = j + 1, n
      r(i,j) = 0.0D+00
    end do
  end do

  do j = 1, n

    s = 0.0D+00

    do k = 1, j - 1
      t = r(k,j) - dot_product ( r(1:k-1,k), r(1:k-1,j) )
      t = t / r(k,k)
      r(k,j) = t
      s = s + t * t
    end do

    s = r(j,j) - s

    if ( s < 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_POFAC - Fatal error!'
      write ( *, '(a)' ) '  The matrix is not positive definite.'
      stop
    end if

    if ( s == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_POFAC - Warning!'
      write ( *, '(a)' ) '  The matrix is not strictly positive definite.'
    end if

    r(j,j) = sqrt ( s )

  end do

  return
end subroutine r8mat_pofac

subroutine r8mat_poinv ( n, r, b )

!*****************************************************************************80
!
!! R8MAT_POINV computes the inverse of a factored positive definite matrix.
!
!  Discussion:
!
!    This routine expects to receive R, the upper triangular factor of A,
!    computed by R8MAT_POFAC, with the property that A = R' * R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, real ( kind = 8 ) R(N,N), the Cholesky factor of A.
!
!    Input, real ( kind = 8 ) B(N,N), the inverse of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) t

  b(1:n,1:n) = r(1:n,1:n)

  do k = 1, n

    b(k,k) = 1.0D+00 / b(k,k)
    b(1:k-1,k) = - b(1:k-1,k) * b(k,k)

    do j = k + 1, n
      t = b(k,j)
      b(k,j) = 0.0D+00
      b(1:k,j) = b(1:k,j) + t * b(1:k,k)
    end do

  end do
!
!  Form inverse(R) * (inverse(R))'.
!
  do j = 1, n
    do k = 1, j - 1
      t = b(k,j)
      b(1:k,k) = b(1:k,k) + t * b(1:k,j)
    end do
    t = b(j,j)
    b(1:j,j) = b(1:j,j) * t
  end do

  return
end subroutine r8mat_poinv

subroutine r8mat_upsol ( n, r, b, x )

!*****************************************************************************80
!
!! R8MAT_UPSOL solves R * X = B for an upper triangular matrix R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) R(N,N), the upper triangular matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side.
!
!    Output, real ( kind = 8 ) X(N), the solution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) x(n)

  x(1:n) = b(1:n)

  x(n) = x(n) / r(n,n)

  do j = n - 1, 1, -1
    x(1:j) = x(1:j) - r(1:j,j+1) * x(j+1)
    x(j) = x(j) / r(j,j)
  end do

  return
end subroutine r8mat_upsol

subroutine r8mat_utsol ( n, r, b, x )

!*****************************************************************************80
!
!! R8MAT_UTSOL solves R' * X = B for an upper triangular matrix R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) R(N,N), the upper triangular matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side.
!
!    Output, real ( kind = 8 ) X(N), the solution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) x(n)

  x(1:n) = b(1:n)

  x(1) = x(1) / r(1,1)

  do j = 2, n
    x(j) = ( x(j) - dot_product ( r(1:j-1,j), x(1:j-1) ) ) / r(j,j)
  end do

  return
end subroutine r8mat_utsol

function r8vec_multinormal_pdf ( n, mu, r, c_det, x )

!*****************************************************************************80
!
!! R8VEC_MULTINORMAL_PDF evaluates a multivariate normal PDF.
!
!  Discussion:
!
!    PDF ( MU(1:N), C(1:N,1:N); X(1:N) ) =
!      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / sqrt ( det ( C ) )
!      * exp ( - ( X - MU )' * inverse ( C ) * ( X - MU ) / 2 )
!
!    Here,
!
!      X is the argument vector of length N,
!      MU is the mean vector of length N,
!      C is an N by N positive definite symmetric covariance matrix.
!
!    The properties of C guarantee that it has an upper triangular
!    matrix R, the Cholesky factor, such that C = R' * R.  It is the
!    matrix R that is required by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) MU(N), the mean vector.
!
!    Input, real ( kind = 8 ) R(N,N), the upper triangular Cholesky
!    factor of the covariance matrix C.
!
!    Input, real ( kind = 8 ) C_DET, the determinant of the
!    covariance matrix C.
!
!    Input, real ( kind = 8 ) X(N), a sample of the distribution.
!
!    Output, real ( kind = 8 ) R8VEC_MULTINORMAL_PDF, the PDF evaluated
!    at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c_det
  real ( kind = 8 ) mu(n)
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) r8vec_multinormal_pdf
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xcx
  real ( kind = 8 ) y(n)
!
!  Compute:
!    inverse(R')*(x-mu) = y
!  by solving:
!    R'*y = x-mu
!
  b(1:n) = x(1:n) - mu(1:n)
  call r8mat_utsol ( n, r, b, y )
!
!  Compute:
!    (x-mu)' * inv(C)          * (x-mu)
!  = (x-mu)' * inv(R'*R)       * (x-mu)
!  = (x-mu)' * inv(R) * inv(R) * (x-mu)
!  = y' * y.
!
  xcx = dot_product ( y, y )

  pdf = 1.0D+00 / sqrt ( ( 2.0D+00 * pi ) ** n ) &
      * 1.0D+00 / sqrt ( c_det ) &
      * exp ( -0.5D+00 * xcx )

  r8vec_multinormal_pdf = pdf

  return
end function r8vec_multinormal_pdf

subroutine r8vec_multinormal_sample ( n, mu, r, x )

!*****************************************************************************80
!
!! R8VEC_MULTINORMAL_SAMPLE samples a multivariate normal PDF.
!
!  Discussion:
!
!    PDF ( MU(1:N), C(1:N,1:N); X(1:N) ) =
!      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / det ( C )
!      * exp ( - ( X - MU )' * inverse ( C ) * ( X - MU ) / 2 )
!
!    Here,
!
!      X is the argument vector of length N,
!      MU is the mean vector of length N,
!      C is an N by N positive definite symmetric covariance matrix.
!
!    The properties of C guarantee that it has an upper triangular
!    matrix R, the Cholesky factor, such that C = R' * R.  It is the
!    matrix R that is required by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) MU(N), the mean vector.
!
!    Input, real ( kind = 8 ) R(N,N), the upper triangular Cholesky
!    factor of the covariance matrix C.
!
!    Output, real ( kind = 8 ) X(N), a sample of the distribution.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mu(n)
  real ( kind = 8 ) r(n,n)
!  real ( kind = 8 ) r8_normal_01_sample
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) z(n)
!
!  Compute X = MU + R' * Z
!  where Z is a vector of standard normal variates.
!
  do j = 1, n
    z(j) = r8_normal_01_sample ( )
  end do

  do i = 1, n
    x(i) = mu(i)
    do j = 1, i
      x(i) = x(i) + r(j,i) * z(j)
    end do
  end do

  return
end subroutine r8vec_multinormal_sample

END MODULE
