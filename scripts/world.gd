extends Node2D


# Called when the node enters the scene tree for the first time.
func _ready():

	# Spawn some random ass items
	InventoryItem.new_item("crap", preload("res://assets/Hardware/asus460_0.png"), Vector2(0, 0))


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass
