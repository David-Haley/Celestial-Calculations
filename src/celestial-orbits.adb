-- Basic Orbital calculations.
-- Algorithms are based on Celestial Calculations by J L Lawrence.

-- Author    : David Haley
-- Created   : 02/02/2023
-- Last Edit : 02/02/2023

package body Celestial.Orbits is

   subtype C_R is Celestial_Real;

   function Orbital_Radius (Semi_Major_Axis : in Celestial_Real;
                            Eccentricity : in Eccentricities;
                            True_Anomaly : in Degrees) return Celestial_Real is
     (Semi_Major_Axis * C_R (1.0 - Eccentricity ** 2) /
      (1.0 + C_R (Eccentricity) * Cos (True_Anomaly)));
   -- Semi_Major_Axis and result are the same units m, km or Light Years etc.

   function Mean_Anomaly (Period, Orbit_Time : Celestial_Real) return Degrees is
     (Degrees (C_R (Full_Circle) * Orbit_Time / Period));
   -- Period and Time are the same units, s, hours, days or years etc.

   function Mean_Anomaly (Period, Orbit_Time : Celestial_Real) return Radians is
     (Two_Pi * Radians (Orbit_Time / Period));
   -- Period and Time are the same units, s, hours, days or years etc.

   Function Orbit_Time (Period : in Celestial_Real;
                        Mean_Anomaly : Degrees) return Celestial_Real is
      (Period * C_R (Mean_Anomaly) / C_R (Full_Circle));
   -- Period and result are the same units, s, hours, days or years etc.

   Function Orbit_Time (Period : in Celestial_Real;
                        Mean_Anomaly : Radians) return Celestial_Real is
      (Period * C_R (Mean_Anomaly / Two_Pi));
   -- Period and result are the same units, s, hours, days or years etc.

   function Eccentric_Anomaly (Eccentricity : in Eccentricities;
                               Mean_Anomaly : in Radians) return Radians is

      -- Solution based on Newton Rapson solution to Kepler's equation.

      Max_Iterations : constant Positive := 100;
      -- arbitary limit to iterations

      Delta_E : Radians := 1.0E-6;
      E, Previous_E : Radians;
      I : Positive := 1;
      Ecc : Celestial_Real := C_R (Eccentricity);
      M : Celestial_Real := C_R (Mean_Anomaly);

   begin -- Eccentric_Anomaly
      if Eccentricity > 0.75 then
         E := Pi;
      else
         E := Mean_Anomaly;
      end if; -- Eccentricity > 0.75
      Previous_E := E;
      loop -- Iterate to reduce error in E
         E := E - Radians ((C_R (E) - Ecc * Sin (E) - M) /
                           (1.0 - C_R (E) * Cos (E)));
         exit when abs (E - Previous_E) <= Delta_E or I >= Max_Iterations;
         Previous_E := E;
         I := I + 1;
      end loop; -- Iterate to reduce error in E
      if I >= Max_Iterations and (E - Previous_E) > Delta_E then
         raise Program_Error with "Eccentric anomaly did not converge";
      end if; -- I >= Max_Iterations and (E - Previous_E) > Delta_E
      return E;
   end Eccentric_Anomaly;

   function True_Anomaly (Eccentricity : in Eccentricities;
                          Mean_Anomaly : in Degrees) return Degrees is

      Ecc : constant Celestial_Real := C_R (Eccentricity);
      E : Radians;

   begin -- True_Anomaly
      if Mean_Anomaly = Degrees (Full_Circle / 2) then
         return Mean_Anomaly; -- Tan (Mean_Anomaly /2) infinite
      else
         E := Eccentric_Anomaly (Eccentricity, To_Radians (Mean_Anomaly));
         return 2.0 * Arctan (Sqrt ((1.0 + Ecc) / (1.0 - Ecc)) * Tan (E / 2.0));
      end if; -- Mean_Anomaly = Degrees (Full_Circle / 2)
   end True_Anomaly;

   function True_Anomaly (Eccentricity : in Eccentricities;
                          Mean_Anomaly : in Radians) return Radians is

      Ecc : constant Celestial_Real := C_R (Eccentricity);
      M : Radians renames Mean_Anomaly;
      E : Radians;

   begin -- True_Anomaly
      if M = Pi then
         return M; -- Tan (M /2) infinite
      else
         E := Eccentric_Anomaly (Eccentricity, Mean_Anomaly);
         return 2.0 * Arctan (Sqrt ((1.0 + Ecc) / (1.0 - Ecc)) * Tan (E / 2.0));
      end if; -- M = Pi
   end True_Anomaly;

end  Celestial.Orbits;
