with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

package body Philosophers is

    protected body Fork is
        entry Get when not Taken is
        begin
            Taken := True;
        end Get;
        entry Put when Taken is
        begin
            Taken := False;
        end Put;
    end Fork;

    procedure Dinner(
        PhilosophersNumber : Positive;
        MealsToEat : Positive
    ) is
        Forks : array ( 1 .. PhilosophersNumber ) of Fork;
        
        task type Philosopher ( meals : Positive ) is
            entry PhilosopherInit ( Label : Positive );
        end Philosopher;

        task body Philosopher is
            PhilosopherId : Integer;
            FirstId, SecondId : Integer;

            Random_Duration : Duration;
            type Custom is range 200..1000;
            package Rand_Cust is new Ada.Numerics.Discrete_Random(Custom);
            use Rand_Cust;
            Seed : Generator;
            Num  : Custom;
        begin
            -- Assigning forks for asymetric solution
            accept PhilosopherInit ( Label : Positive ) do
                PhilosopherId := Label;
                FirstId := Label;
                SecondId := Label - 1;
                if SecondId = 0 then
                    FirstId := PhilosophersNumber;
                    SecondId := Label;
                end if;
            end PhilosopherInit;
            Put_Line ("Philosopher" & Integer'Image(PhilosopherId) & " assigned to forks" & Integer'Image(FirstId) & " and " & Integer'Image(SecondId));
            delay(0.3);
            for I in 1 .. meals loop
                Reset(Seed);
                Num := Random(Seed);
                Random_Duration := Duration(Num) / 1000.0;

                Forks(FirstId).Get;
                Put_Line ("[" & Positive'Image(PhilosopherId) & "] takes fork number" & Integer'Image(FirstId));
                Forks(SecondId).Get;
                Put_Line ("[" & Positive'Image(PhilosopherId) & "] takes fork number" & Integer'Image(SecondId));
                
                Put_Line ("    [" & Positive'Image(PhilosopherId) & "] eats meal number" & Positive'Image(I));
                delay(Random_Duration);

                Forks(SecondId).Put;
                Put_Line ("  [" & Positive'Image(PhilosopherId) & "] puts down fork number" & Integer'Image(SecondId));
                Forks(FirstId).Put;
                Put_Line ("  [" & Positive'Image(PhilosopherId) & "] puts down fork number" & Integer'Image(FirstId));
            end loop;
        end Philosopher;

        Philosophers : array ( Natural range 1 .. PhilosophersNumber ) of Philosopher ( MealsToEat );

    begin
        for I in Natural range 1 .. PhilosophersNumber loop
            Philosophers(I).PhilosopherInit(I);
        end loop;
        Put_Line("");
    end Dinner;

end Philosophers;