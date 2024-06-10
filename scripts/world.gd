extends Node2D

# ! AUTOLOADED as World
# yes, WorldNode it's also and autoloaded varible

@onready var ItemLayer = WorldNode.get_node("ItemLayer")

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
