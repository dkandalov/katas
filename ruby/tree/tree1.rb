class Node
  attr_reader :name, :left, :right

  def initialize(name, left, right)
    @name, @left, @right = name, left, right
  end

  def inspect
    "#@name (#{@left.inspect}; #{@right.inspect})"
  end
end

def walk_tree(node)
  return if node.nil?

  p node.name
  walk_tree node.left
  walk_tree node.right
end

root = Node.new("root",
                Node.new("1",
                         Node.new("11", nil, nil),
                         Node.new("12", nil, nil)), 
                Node.new("2", nil, nil)
       )
walk_tree root
