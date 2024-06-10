# This is named "InventoryItem" and not just "item" because "Item" conflicts with the built-in "Item" class in Godot.
# Prepare youself for a bunch of inconsistencies in the code, as I was learning as I went along.

@tool
extends Node2D
class_name InventoryItem 

# Item data
var data: ItemData 
@export var texture: Texture # For the editor, this is not really needed in the game

const scene_path: String = "res://scenes/inventory_item.tscn"
const scene = preload(scene_path)

# Scene-Tree Node references
@onready var icon_sprite = $Sprite2D

# Variables
var player_in_range = false

static func new_item(data: ItemData, pos: Vector2) -> InventoryItem:
	var ins = scene.instantiate()
	ins.data = data
	ins.global_position = pos
	return ins

func _ready():
	# Set the texture to reflect in the game
	if not Engine.is_editor_hint():
		icon_sprite.texture = data.texture
	else:
		icon_sprite.texture = texture

# Add item to inventory
func pickup_item():
	Global.add_item(data)
	self.queue_free()
