package Philosophers is

    protected type Fork is
        entry Get;
        entry Put;
    private
        Taken : Boolean := False;
    end Fork;

    procedure Dinner (
        PhilosophersNumber : Positive;
        MealsToEat : Positive
    );

end Philosophers;
