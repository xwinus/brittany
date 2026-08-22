class Render value where
  -- class method comment
  render :: value -> String
instance Render Int where
  -- instance method comment
  render value = show value
