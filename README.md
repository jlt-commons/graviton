# graviton

Graviton is a physics based game where the player collects green prizes while avoiding red death zones.

The player adds gravitational attractors on the screen by clicking and dragging the mouse,
and the ship follows the gravity fields. The more gravitational attractors are added the more
death zones will be spawned on the screen.

## running

    jolt -M:run

## tests

    jolt -M:test

The suite runs unit tests plus [writ](https://github.com/jlt-commons/writ)
specs: `graviton.physics-spec` and
`graviton.world-spec` state the field math and the round's state graph as
checkable laws.
