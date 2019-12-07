# Approximation of PI

In this example, we will try to estimate the value of π by using a [Monte-Carlo](https://en.wikipedia.org/wiki/Monte_Carlo_method) approach. In order to have a good estimation, we will have to generate many points in a two dimensional space.

Let X = "The x coordinate of a given point"

X ~ Un(0, 1)

Let Y = "The y coordinate of a given point"

Y ~ Un(0, 1)

Each point will be represented with a tuple (X, Y).

Let

- S = "The surface of the circle"
- r = "The radius of the circle"

We know that the surface of a circle is given by the following equation:

S = πr²

If r is equal to 1, then:

S = π

π = 4 . (S / 4)

We can estimate S / 4 by computing the ratio between the number of points whose distance from the origin is less or equal than 1 and the number of sampling. By multiplying this ratio by 4, we will end up with an estimation of π.

If each node performs a sampling and shares its results with the other nodes, each node can have a better approximation of π.

## Achlys task model

Here is how we can declare a new task in Achlys:

```erlang
Task = achlys:declare(mytask, all, single, fun() ->

    % Declare variable :

    Type = state_gset,
    Set = {<<"set">>, Type},
    lasp:declare(Set, Type),

    % Add listeners :

    lasp:stream(Set, fun(S) ->
        R = lists:foldl(fun(Current, Acc) ->
                case Current of #{n := N1, success := S1} ->
                    case Acc of #{n := N2, success := S2} -> #{
                        n => N1 + N2,
                        success => S1 + S2
                    }
                    end
                end
            end,
            #{n => 0, success => 0},
            sets:to_list(S)
        ),
        case R of #{n := N, success := Success} ->
            io:format("Grow Only Set : ~p~n", [S]),
            io:format("PI ≃ ~p~n", [4 * Success / N])
        end
    end),

    % Helper functions :

    GetRandomPoint = fun() -> {
        rand:uniform(),
        rand:uniform()
    } end,

    GetDistance = fun(Point) -> 
        case Point of {X, Y} ->
            math:sqrt(X * X + Y * Y)
        end
    end,

    % Sampling :

    N = 450,
    Success = lists:foldl(fun(_, Count) -> 
        case GetDistance(GetRandomPoint()) of
            Distance when Distance =< 1 -> Count + 1;
            _ -> Count
        end    
    end, 0, lists:seq(1, N)),

    % Update :

    lasp:update(Set, {add, #{
        n => N,
        success => Success
    }}, self())
end)
```

A node can listen to changes of a variable by using the `stream` function of `lasp`. This function takes another function as argument that will be called whenever the targeting CRDT variable is updated. The argument of the function corresponds to the new variable value.