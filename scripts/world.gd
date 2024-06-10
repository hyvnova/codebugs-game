extends Node2D

"""
World is the medium between code and the game world.
Anything that needs to be spawned, or added to the world, should be done through this script.

World -> This script
WorldNode -> The root node. 
"""


@onready var ItemLayer = WorldNode.get_node("ItemLayer")
@onready var EntityLayer = WorldNode.get_node("EntityLayer")

var rng = RandomNumberGenerator.new()

# Called when the node enters the scene tree for the first time.
func _ready():
	
	const texture = preload("res://assets/Hardware/asus460_0.png")

	# Spawn some random ass items 
	spawn_item(
		ItemData.new_data("crap", texture), 
		Player.global_position # random position nearby player
	)


func spawn_item(data: ItemData, pos: Vector2):
	ItemLayer.add_child(InventoryItem.new_item(data, pos))
	print("Spawning %sx %s" % [data.quantity, data.type]) 

func spawn(entity):
	"""
	Adds `entity` to EntityLayer, in other words it becomes part of the world
	"""
	EntityLayer.add_child(entity)

