-record(avatar, {
    id,
    type,
    energy = 0,
    health = 0,
    food = 0,
    spear,
    age = 0,
    kills = 0,
    loc,
    direction,
    r,
    mass,
    objects = [],
    state,
    stats,
    actuators,
    sensors
}).

-record(obj, {
    type, % circle, line
    id = undefined,
    color,
    pivot, % 枢轴
    coords = [],
    params = void
}).

