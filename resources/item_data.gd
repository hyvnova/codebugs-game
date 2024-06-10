extends Resource
class_name ItemData

var texture: Texture
var type: String
var quantity: int

static func new_data( type: String, texture: Texture, quantity: int = 1) -> ItemData:
	var item_data = ItemData.new()
	item_data.texture = texture
	item_data.type = type
	item_data.quantity = quantity
	return item_data

func clone() -> ItemData:
	return ItemData.new_data( type, texture, quantity)
