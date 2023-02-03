-- Basic Orbital calculations.
-- Algorithms are based on Celestial Calculations by J L Lawrence.

-- Author    : David Haley
-- Created   : 02/02/2023
-- Last Edit : 02/02/2023

with Celestial; use Celestial;

package Celestial.Orbits is

   type Eccentricities is new Celestial_Real range 0.0 .. 1.0;

   function Orbital_Radius (Semi_Major_Axis : in Celestial_Real;
                            Eccentricity : in Eccentricities;
                            True_Anomaly : in Degrees) return Celestial_Real;
   -- Semi_Major_Axis and result are the same units m, km or light years etc.

   function Mean_Anomaly (Period, Orbit_Time : Celestial_Real) return Degrees;
   -- Period and Orbit_Time are the same units, s, hours, days or years etc.

   function Mean_Anomaly (Period, Orbit_Time : Celestial_Real) return Radians;
   -- Period and Orbit_Time are the same units, s, hours, days or years etc.

   Function Orbit_Time (Period : in Celestial_Real;
                        Mean_Anomaly : Degrees) return Celestial_Real;
   -- Period and result are the same units, s, hours, days or years etc.

   Function Orbit_Time (Period : in Celestial_Real;
                        Mean_Anomaly : Radians) return Celestial_Real;
   -- Period and result are the same units, s, hours, days or years etc.

   function True_Anomaly (Eccentricity : in Eccentricities;
                          Mean_Anomaly : in Degrees) return Degrees;
   -- Uses seventh order equation of the centre from Wikipedia

   function True_Anomaly (Eccentricity : in Eccentricities;
                          Mean_Anomaly : in Radians) return Radians;
   -- Uses seventh order equation of the centre from Wikipedia

end  Celestial.Orbits;
