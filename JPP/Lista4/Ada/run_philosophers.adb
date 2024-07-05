
with Ada.Command_Line; use Ada.Command_Line;
with Philosophers; use Philosophers;

procedure run_philosophers is
begin
    if Argument_Count = 2 then

        Dinner ( 
            Positive'Value( Argument(1) ),
            Positive'Value( Argument(2) )
        );

    end if;
end run_philosophers;