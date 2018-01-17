package.path = package.path .. ";../?.lua"
require("common")

----------------------
-- Roll Your Own OO --
----------------------
dietrich = {
  name = "Dietrich",
  health = 100,

  take_hit = function(self)
    self.health = self.health - 10
  end
}
clone = {
   name     = dietrich.name,
   health   = dietrich.health,
   take_hit = dietrich.take_hit
}

dietrich.take_hit(dietrich)
expect_to_be_equal(dietrich.health, 90)
expect_to_be_equal(clone.health, 100)

----------------
-- Prototypes --
----------------
Villain = { health = 100 }

function Villain:new(name)
  local obj = {
    name = name,
    health = self.health,
  }
  setmetatable(obj, self)
  self.__index = self
  return obj
end

function Villain:take_hit()
  self.health = self.health - 10
end

dietrich = Villain:new("Dietrich")
Villain.take_hit(dietrich)
expect_to_be_equal(dietrich.health, 90)
dietrich:take_hit()
expect_to_be_equal(dietrich.health, 80)


-----------------
-- Inheritance --
-----------------
SuperVillain = Villain:new()
function SuperVillain:take_hit()
  -- Haha, armor!
  self.health = self.health - 5
end
toht = SuperVillain:new("Toht")
toht:take_hit()
expect_to_be_equal(toht.health, 95)
