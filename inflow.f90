
FUNCTION InFlow( Model, n, x ) RESULT( vin )
  USE Types
  TYPE(Model_t) :: Model
  INTEGER :: n
  REAL(KIND=dp) :: y1,z1,vin,v0,vt
  
  y1 = Model % Nodes % y(n) 
  z1 = Model % Nodes % z(n)
  write (*,*) y1, z1
  y1 = y1 - 0.5
  v0 = sqrt(y1**2 + z1**2)
  Write (*,*) "V0 =",v0
  If (v0 .le. 0.001) then
   vin = .005
   Write(*,*) "my VIN:",vin
   return
  end if

  IF(v0 .ge. 0.499) THEN
    vin = 0.0
  ELSE
    vin = .005 - ((v0/.05)*.005)
  END IF

  Write(*,*) "VIN:",vin

END FUNCTION InFlow

