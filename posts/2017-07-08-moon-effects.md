---
title: Astronomical effects in Moonrise/Moonset calculations
author: Alexander Ignatyev
tags: astro, haskell
---
Among the other visible celestial objects the Moon is one of the most difficult to calculate.

The are a number of reasons:

* The moon is affected by the gravity of the Earth and the Sun;

* it is a very close objects so we need to take into account the parallax effect;

* it is a big object in terms of angular size (true for the Sun as well);

* the refraction effect due to the Earth's atmosphere (true for all celestial objects).


Let's take into account all these effect.

First of all, define some of the values we will use below:

```Haskell
london :: GeographicCoordinates
london = GeoC 51.5074 (-0.1278)

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 7 6 10 30 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 7 6

jd :: JulianDate
jd = lctUniversalTime dt

-- distance from the Earth to the Moon in kilometres
mdu :: MoonDistanceUnits
mdu = moonDistance1 j2010MoonDetails jd
-- MDU 0.9550170577020396

distance :: Double
distance = mduToKm mdu
-- 367109.51199772174

-- Angular Size
angularSize :: DecimalDegrees
angularSize = moonAngularSize mdu
-- DD 0.5425033990980761
```

To calculate refraction we can use `refract` function from `Data.Astro.Effects` module. It takes the observed altitude, temperature in degrees centigrade and barometric pressure in millibars.

The observed altitude is always 0 if we calculate rise and set, good reasonable values for temperature and barometric pressure are 12 and 1013 respectively:

```Haskell
r = refract 0 12 1013
-- DD 0.5665691228070175
```

Angular size correction is simple:

```Haskell
s = 0.5 * angularSize
DD 0.24557263666302662
```


The refraction effect and angular size correction alter the apparent height of the object, we will use sum of them as a parameter of `riseAndSet2` function:

```Haskell
verticalShift :: DecimalDegrees
verticalShift = r + s
  where r = refract 0 12 1012
        s = 0.5 * angularSize
-- DD 0.8115824612244301
```

Let's calculate the Parallax effect now. `parallax` function of `Data.Astro.Effects` module "corrects" given equatorial coordinates, apart the coordinates of the celestial object it takes geographic coordinates of the observer and height above sea-level of the observer measured in metres, distance from the celestial object to the Earth measured in AU and the Universal Time:

```haskell
distance = moonDistance1 j2010MoonDetails jd
p = parallax london 20 distance jd (EC1 0 0)
-- EC1 { e1Declination = DD (-0.7020801857390149)
--     , e1RightAscension = DH (-3.447510112899046e-2) }
```

Fortunately, `moonPosition2` function (which takes the parallax effect into account) is available in astro library since version 0.4.2.0.


Now let's summarise everything we talked about and calculate Moonrise and Moonset time:

```haskell
module Main where

import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Moon
import Data.Astro.Moon.MoonDetails

london :: GeographicCoordinates
london = GeoC 51.5074 (-0.1278)

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 7 6 10 30 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 7 6

jd :: JulianDate
jd = lctUniversalTime dt

-- distance from the Earth to the Moon in kilometres
mdu :: MoonDistanceUnits
mdu = moonDistance1 j2010MoonDetails jd

distance :: Double
distance = mduToKm mdu

-- Angular Size
angularSize :: DecimalDegrees
angularSize = moonAngularSize mdu

position :: JulianDate -> EquatorialCoordinates1
position jd = moonPosition2 j2010MoonDetails distance london height jd
  where distance = moonDistance1 j2010MoonDetails jd
        height = 20


verticalShift :: DecimalDegrees
verticalShift = r + s
  where r = refract 0 12 1012
        s = 0.5 * angularSize

riseSet :: RiseSetMB
riseSet = riseAndSet2 0.000001 position london verticalShift today

main :: IO ()
main = print riseSet

-- RiseSet
--  (Just (2017-07-06 18:48:44.5949 +1.0,DD 120.13250305991525))
--  (Just (2017-07-06 03:09:54.7904 +1.0,DD 241.7780054101992))
```

[timeamddate.com](https://www.timeanddate.com/moon/uk/london) gave the following results:

Moonrise: 18:49, (120°)

Moonset: 03:10, (242°)

Those perfectly match with our results.
