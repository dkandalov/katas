class String
  def sanitize
    self.gsub(/[^\w\s]/, "")
  end
end