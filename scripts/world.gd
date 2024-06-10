extends Node2D


@onready var ItemLayer = $ItemLayer

var rng = RandomNumberGenerator.new()

# Called when the node enters the scene tree for the first time.
func _ready():

	const texture = preload("res://assets/Hardware/asus460_0.png")

	# Spawn some random ass items 
	for i in range(10):
		var item = InventoryItem.new_item("crap", texture, Player.global_position)

		ItemLayer.add_child(item)
