@tool
extends Node2D
class_name InventoryItem 

# Item details for editor window
@export var item_type = ""
@export var item_texture: Texture

const scene_path: String = "res://scenes/inventory_item.tscn"
const scene = preload(scene_path)

# Scene-Tree Node references
@onready var icon_sprite = $Sprite2D

# Variables
var player_in_range = false


static func new_item(type: String, texture: Texture, pos: Vector2) -> InventoryItem:
	var ins = scene.instantiate()

	# Set the items values for spawning
	ins.item_type = type
	ins.item_texture = texture

	ins.global_position = pos

	return ins

func _ready():
	# Set the texture to reflect in the game
	if not Engine.is_editor_hint():
		icon_sprite.texture = item_texture

func _process(_delta):
	# Set the texture to reflect in the editor
	if Engine.is_editor_hint():
		icon_sprite.texture = item_texture
	# Add item to inventory if player presses "E" within range
	if player_in_range and Input.is_action_just_pressed("use"):
		pickup_item()

# Add item to inventory
func pickup_item():
	var item = {
		"quantity": 1,
		"type": item_type,
		"texture": item_texture,
		"scene_path": scene_path
	}
	if Global.player_node:
		Global.add_item(item)
		self.queue_free()

# If player is in range, show UI and make item pickable
func _on_area_2d_body_entered(body):
	if body.is_in_group("player"):
		player_in_range = true
		body.interact_ui.visible = true

# If player is in range, hide UI and don't make item pickable
func _on_area_2d_body_exited(body):
	if body.is_in_group("player"):
		player_in_range = false
		body.interact_ui.visible = false
		
