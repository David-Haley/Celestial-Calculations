-- Test program for Celestial.Orbits
-- Author    : David Haley
-- Created   : 02/02/2023
-- Last Edit : 04/02/2023

--20230204 : Spelling correction Anomly to Anomaly

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Celestial; use Celestial;
with Celestial.Orbits; use Celestial.Orbits;

procedure Test_Orbits is

   package C_R_IO is new Ada.Text_IO.Float_IO (Celestial_Real);
   package E_IO is new Ada.Text_IO.Float_IO (Eccentricities);
   package Degree_IO is new Ada.Text_IO.Float_IO (Degrees);
   package Radian_IO is new Ada.Text_IO.Float_IO (Radians);

   procedure Orbital_Radius is

      Semi_Major_Axis : Celestial_Real;
      Eccentricity : Eccentricities;
      True_Anomaly : Degrees;

   begin -- Orbital_Radius
      Put ("Semi Major Axis: ");
      C_R_IO.Get (Semi_Major_Axis);
      Put ("Eccentricity: ");
      E_IO.Get (Eccentricity);
      Put ("True anomaly: ");
      Degree_IO.Get (True_Anomaly);
      Put_Line ("orbital Radius:" &
                  Orbital_Radius (Semi_Major_Axis, Eccentricity,
                  True_Anomaly)'Img);
   end Orbital_Radius;

   procedure Mean_Anomaly_Degrees is

      Period, Orbit_Time : Celestial_Real;
      Result : Degrees;

   begin -- Mean_Anomaly_Degrees
      Put ("Period: ");
      C_R_IO.Get (Period);
      Put ("Orbit time: ");
      C_R_IO.Get (Orbit_Time);
      Result := Mean_Anomaly (Period, Orbit_Time);
      Put_Line ("Mean anomaly:" & Result'Img);
   end Mean_Anomaly_Degrees;

   procedure Mean_Anomaly_Radians is

      Period, Orbit_Time : Celestial_Real;
      Result : Radians;

   begin -- Mean_Anomaly_Radians
      Put ("Period: ");
      C_R_IO.Get (Period);
      Put ("Orbit time: ");
      C_R_IO.Get (Orbit_Time);
      Result := Mean_Anomaly (Period, Orbit_Time);
      Put_Line ("Mean anomaly:" & Result'Img);
   end Mean_Anomaly_Radians;

   procedure Orbit_Time_Degrees is

      Period : Celestial_Real;
      Mean_Anomaly : Degrees;

   begin -- Orbit_Time_Degrees
      Put ("Period: ");
      C_R_IO.Get (Period);
      Put ("Mean anomaly: ");
      Degree_IO.Get (Mean_Anomaly);
      Put_Line ("Orbit time:" & Orbit_Time(Period, Mean_Anomaly)'Img);
   end Orbit_Time_Degrees;

   procedure Orbit_Time_Radians is

      Period : Celestial_Real;
      Mean_Anomaly : Radians;

   begin -- Orbit_Time_Radians
      Put ("Period: ");
      C_R_IO.Get (Period);
      Put ("Mean anomaly: ");
      Radian_IO.Get (Mean_Anomaly);
      Put_Line ("Orbit time:" & Orbit_Time(Period, Mean_Anomaly)'Img);
   end Orbit_Time_Radians;

   procedure True_Anomaly_Degrees is

      Mean_Anomaly : Degrees;
      Eccentricity : Eccentricities;

   begin -- True_Anomaly_Degrees
      Put ("Mean anomaly: ");
      Degree_IO.Get (Mean_Anomaly);
      Put ("Eccentricity: ");
      E_IO.Get (Eccentricity);
      Put_Line ("True anomaly:" &
                  True_Anomaly (Eccentricity, Mean_Anomaly)'Img);
   end True_Anomaly_Degrees;

   procedure True_Anomaly_Radians is

      Mean_Anomaly : Radians;
      Eccentricity : Eccentricities;

   begin -- True_Anomaly_Radians
      Put ("Mean anomaly: ");
      Radian_IO.Get (Mean_Anomaly);
      Put ("Eccentricity: ");
      E_IO.Get (Eccentricity);
      Put_Line ("True anomaly:" &
                  True_Anomaly (Eccentricity, Mean_Anomaly)'Img);
   end True_Anomaly_Radians;

   Test : Character;

begin -- Test_Orbits
   loop -- Perform one test
      Put_Line ("A Orbital_Radius");
      Put_Line ("B Mean Anomaly (Degrees)");
      Put_Line ("C Mean Anomaly (Radians)");
      Put_Line ("D Orbit Time (Degrees)");
      Put_Line ("E Orbit_Time (Radians)");
      Put_Line ("F True Anomaly (Degrees)");
      Put_Line ("G True Anomaly (Radians)");
      Put_Line ("0 Exit");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case To_Upper (Test) is
         when 'A' =>
            Orbital_Radius;
         when 'B' =>
            Mean_Anomaly_Degrees;
         when 'C' =>
            Mean_Anomaly_Radians;
         when 'D' =>
            Orbit_Time_Degrees;
         when 'E' =>
            Orbit_Time_Radians;
         when 'F' =>
            True_Anomaly_Degrees;
         when 'G' =>
            True_Anomaly_Radians;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- To_Upper (Test)
   end loop; -- Perform one test
end Test_Orbits;
