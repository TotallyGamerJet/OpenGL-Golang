package main

import (
	"runtime"
	"github.com/go-gl/glfw/v3.2/glfw"
	"github.com/go-gl/gl/v4.5-core/gl"
	"log"
	"fmt"
	"strings"
	"os"
	"image"
	"image/draw"
	_ "image/png"
	"github.com/go-gl/mathgl/mgl32"
	"math"
	"bufio"
	"io"
	"strconv"
	"io/ioutil"
)

const (
	WIDTH = 1280
	HEIGHT = 720
	TITLE = "OpenGL Tut"

	FOV = 70
	NEAR_PLANE = 0.1
	FAR_PLANE = 1000
)

var (
	window *glfw.Window

	vaos []*uint32
	vbos []*uint32
	textures []*uint32

	cameraXTemp, cameraYTemp, cameraZTemp float32

	entities = make(map[TexturedModel][]Entity)
)

type Light struct {
	position mgl32.Vec3
	colour mgl32.Vec3
}

type Camera struct {
	position mgl32.Vec3
	pitch, yaw, roll float32
}

type Entity struct {
	model TexturedModel
	position mgl32.Vec3
	rotX, rotY, rotZ, scale float32
}

type TexturedModel struct {
	rawModel RawModel
	texture ModelTexture
}

type ModelTexture struct {
	textureID uint32
	shineDamper, reflectivity float32 //DEFAULTS = 1, 0
}

type RawModel struct {
	vaoID uint32
	vertexCount int32
}

type Program struct {
	programID, vertexID, fragmentID uint32
	uniformLocations map[string]int32
}

func (c *Camera) move() {
	//MOVE CAMERA & INPUT
	if cameraXTemp != 0 || cameraYTemp != 0 || cameraZTemp != 0 {
		c.position = c.position.Add(mgl32.Vec3{cameraXTemp, cameraYTemp, cameraZTemp})
		cameraXTemp = 0
		cameraYTemp = 0
		cameraZTemp = 0
	}
}

func (e *Entity) increasePosition(dx, dy, dz float32) {
	e.position = e.position.Add(mgl32.Vec3{dx, dy, dz})
}

func (e *Entity) increaseRotation(dx, dy, dz float32) {
	e.rotX += dx
	e.rotY += dy
	e.rotZ += dz
}

func init() {
	// GLFW event handling must run on the main OS thread
	runtime.LockOSThread()
}

func check(err error) {
	if err !=  nil {
		panic(err)
	}
}

func onKey(window *glfw.Window, k glfw.Key, s int, action glfw.Action, mods glfw.ModifierKey) {

	if window.GetKey(glfw.KeyW) == glfw.Press {
		cameraZTemp -= 0.2
	}
	if window.GetKey(glfw.KeyS) == glfw.Press {
		cameraZTemp += 0.2
	}
	if window.GetKey(glfw.KeyA) == glfw.Press {
		cameraXTemp -= 0.2
	}
	if window.GetKey(glfw.KeyD) == glfw.Press {
		cameraXTemp += 0.2
	}
	if window.GetKey(glfw.KeySpace) == glfw.Press {
		cameraYTemp += 0.2
	}
	if window.GetKey(glfw.KeyLeftShift) == glfw.Press {
		cameraYTemp -= 0.2
	}

}

func initDisplay() *glfw.Window {
	glfw.WindowHint(glfw.Resizable, glfw.False)
	glfw.WindowHint(glfw.ContextVersionMajor, 4)
	glfw.WindowHint(glfw.ContextVersionMinor, 5)
	glfw.WindowHint(glfw.OpenGLProfile, glfw.OpenGLCoreProfile)
	glfw.WindowHint(glfw.OpenGLForwardCompatible, glfw.True)
	window, err := glfw.CreateWindow(WIDTH, HEIGHT, TITLE, nil, nil)
	check(err)
	window.MakeContextCurrent()
	return window
}

func prepare() {
	gl.Enable(gl.DEPTH_TEST)
	gl.ClearColor(1.0, 0, 0, 1.0)
	gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
}

func processEntity(entity Entity) {
	entityModel := entity.model
	batch, _ := entities[entityModel]
	if batch != nil{
		batch = append(batch, entity)
	} else {
		var newBatch []Entity
		newBatch = append(newBatch, entity)
		entities[entityModel] = newBatch
	}
}

func render(sun Light, camera Camera, program Program) {
	prepare()
	loadLight(sun, program)
	loadViewMatrix(camera, program)
	renderEntities(entities, program)
	entities = make(map[TexturedModel][]Entity)
}

func renderEntities(entities map[TexturedModel][]Entity, program Program) {
	for model, batch := range entities {
		prepareTexturedModel(model, program)
		for _, entity := range batch {
			prepareInstance(entity, program)
			gl.DrawElements(gl.TRIANGLES, model.rawModel.vertexCount, gl.UNSIGNED_INT, gl.PtrOffset(0))

		}
		unbindTexturedModel()
	}
}

func prepareTexturedModel(model TexturedModel, program Program) {
	rawmodel := model.rawModel
	gl.BindVertexArray(rawmodel.vaoID)
	gl.EnableVertexAttribArray(0)
	gl.EnableVertexAttribArray(1)
	gl.EnableVertexAttribArray(2)
	texture := model.texture
	loadShineVariables(texture.shineDamper, texture.reflectivity, program)
	gl.ActiveTexture(gl.TEXTURE0)
	gl.BindTexture(gl.TEXTURE_2D, texture.textureID)
}

func unbindTexturedModel() {
	gl.DisableVertexAttribArray(0)
	gl.DisableVertexAttribArray(1)
	gl.DisableVertexAttribArray(2)
	gl.BindVertexArray(0)
}

func prepareInstance(entity Entity, program Program) {
	transformationMatrix := createTransformationMatrix(entity.position, entity.rotX, entity.rotY, entity.rotZ, entity.scale)
	gl.UniformMatrix4fv(program.uniformLocations["location_transformationMatrix"], 1,false, &transformationMatrix[0])

}

var projectionMatrix mgl32.Mat4
func createProjectionMatrix(program Program) {
	projectionMatrix = mgl32.Ident4()
	aspectRatio := float32(WIDTH)/HEIGHT
	projectionMatrix = mgl32.Perspective(mgl32.DegToRad(FOV), aspectRatio, NEAR_PLANE, FAR_PLANE)
	gl.UniformMatrix4fv(program.uniformLocations["location_projectionMatrix"], 1,false, &projectionMatrix[0])
}

func loadToVAO(positions, textureCoords, normals []float32, indices []int) RawModel {
	vaoID := createVAO()
	bindIndicesVBO(indices)
	storeDataInAttributeList(0, 3, positions)
	storeDataInAttributeList(1, 2, textureCoords)
	storeDataInAttributeList(2, 3, normals)
	gl.BindVertexArray(0)
	return RawModel{vaoID, int32(len(indices))}
}

func toRadian(degree float32) float32 {
	return degree * math.Pi / 180
}

func loadTexture(file string) (uint32, error) {
	imgFile, err := os.Open(file)
	if err != nil {
		return 0, fmt.Errorf("texture %q not found on disk: %v", file, err)
	}
	img, _, err := image.Decode(imgFile)
	if err != nil {
		return 0, err
	}

	rgba := image.NewRGBA(img.Bounds())
	if rgba.Stride != rgba.Rect.Size().X*4 {
		return 0, fmt.Errorf("unsupported stride")
	}
	draw.Draw(rgba, rgba.Bounds(), img, image.Point{0, 0}, draw.Src)

	var texture uint32
	gl.GenTextures(1, &texture)
	textures = append(textures, &texture)
	gl.ActiveTexture(gl.TEXTURE0)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
	gl.TexImage2D(
		gl.TEXTURE_2D,
		0,
		gl.RGBA,
		int32(rgba.Rect.Size().X),
		int32(rgba.Rect.Size().Y),
		0,
		gl.RGBA,
		gl.UNSIGNED_BYTE,
		gl.Ptr(rgba.Pix))

	return texture, nil
}

func createVAO() uint32{
	var vao uint32
	gl.GenVertexArrays(1, &vao)
	vaos = append(vaos, &vao)
	gl.BindVertexArray(vao)
	return vao
}

func storeDataInAttributeList(attribnum uint32, coordSize int32, data []float32) {
	var vbo uint32
	gl.GenBuffers(1, &vbo)//bind
	vbos = append(vbos, &vbo)

	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.BufferData(gl.ARRAY_BUFFER, len(data)*4, gl.Ptr(data), gl.STATIC_DRAW)
	gl.VertexAttribPointer(attribnum, coordSize, gl.FLOAT, false, 0/**(5*4)*/, gl.PtrOffset(0))

	gl.BindBuffer(gl.ARRAY_BUFFER, 0)//unbind
}

func bindIndicesVBO(indices []int) {
	var vbo uint32
	gl.GenBuffers(1, &vbo)//bind
	vbos = append(vbos, &vbo)
	gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbo)
	gl.BufferData(gl.ELEMENT_ARRAY_BUFFER, len(indices)*4, gl.Ptr(indices), gl.STATIC_DRAW)
}

func cleanUp() {
	for _, vao := range vaos {
		gl.DeleteVertexArrays(1, vao)
	}
	for _, vbo := range vbos {
		gl.DeleteBuffers(1, vbo)
	}
	for _, texture := range textures {
		gl.DeleteTextures(1, texture)
	}
}

func loadOBJModel(filename string) RawModel {
	f, err := os.Open("res/" + filename)
	check(err)
	defer f.Close()

	r := bufio.NewReader(f)

	var line string
	var currentLine []string
	var s []byte

	var vertices []mgl32.Vec3
	var textures []mgl32.Vec2
	var normals []mgl32.Vec3
	var indices []int
	var verticesArray []float32
	var textureArray []float32
	var normalsArray []float32

	var x, y, z float64
	var vertex1, vertex2, vertex3 []string

	for {
		s, _, err = r.ReadLine()
		check(err)
		line = string(s[:])
		currentLine = strings.Split(line, " ")

		if strings.HasPrefix(line, "v ") {
			x, err = strconv.ParseFloat(currentLine[1], 64)
			check(err)
			y, err = strconv.ParseFloat(currentLine[2], 64)
			check(err)
			z, err = strconv.ParseFloat(currentLine[3], 64)
			check(err)

			vertices = append(vertices, mgl32.Vec3{float32(x), float32(y), float32(z)})
		} else if strings.HasPrefix(line, "vt ") {
			x, err = strconv.ParseFloat(currentLine[1], 64)
			check(err)
			y, err = strconv.ParseFloat(currentLine[2], 64)
			check(err)

			textures = append(textures, mgl32.Vec2{float32(x), float32(y)})
		} else if strings.HasPrefix(line, "vn ") {
			x, err = strconv.ParseFloat(currentLine[1], 64)
			check(err)
			y, err = strconv.ParseFloat(currentLine[2], 64)
			check(err)
			z, err = strconv.ParseFloat(currentLine[3], 64)
			check(err)

			normals = append(normals, mgl32.Vec3{float32(x), float32(y), float32(z)})
		} else if strings.HasPrefix(line, "f ") {
			textureArray = make([]float32, len(vertices) * 2)
			normalsArray = make([]float32, len(vertices) * 3)
			break
		}
	}

	for err != io.EOF {
		if !strings.HasPrefix(line, "f ") {
			s, _, err = r.ReadLine()
			check(err)
			line = string(s[:])
			continue
		}
		currentLine = strings.Split(line, " ")

		vertex1 = strings.Split(currentLine[1], "/")
		vertex2 = strings.Split(currentLine[2], "/")
		vertex3 = strings.Split(currentLine[3], "/")

		indices, textureArray, normalsArray = processVertex(vertex1, indices, textures, normals, textureArray, normalsArray)
		indices, textureArray, normalsArray = processVertex(vertex2, indices, textures, normals, textureArray, normalsArray)
		indices, textureArray, normalsArray = processVertex(vertex3, indices, textures, normals, textureArray, normalsArray)
		s, _, err = r.ReadLine()
		line = string(s[:])
	}

	for _, vertex := range vertices {
		verticesArray = append(verticesArray, vertex.X(), vertex.Y(), vertex.Z())
	}

	return loadToVAO(verticesArray, textureArray, normalsArray, indices)
}

func processVertex(vertexData []string , indices []int, textures []mgl32.Vec2,
	normals[]mgl32.Vec3,  textureArray,  normalsArray []float32) ([]int, []float32, []float32) {
	currentVertexPointer, err := strconv.ParseInt(vertexData[0], 10, 64)
	check(err)
	currentVertexPointer--
	indices = append(indices, int(currentVertexPointer))
	texCoord, err := strconv.ParseInt(vertexData[1], 10, 64)
	check(err)
	currentTex := textures[texCoord - 1]
	textureArray[currentVertexPointer * 2] = currentTex.X()
	textureArray[currentVertexPointer * 2 + 1] = 1 - currentTex.Y()
	normCoord, err := strconv.ParseInt(vertexData[2], 10,64)
	check(err)
	currentNorm := normals[normCoord - 1]
	normalsArray[currentVertexPointer * 3] = currentNorm.X()
	normalsArray[currentVertexPointer * 3 + 1] = currentNorm.Y()
	normalsArray[currentVertexPointer * 3 + 2] = currentNorm.Z()
	return indices, textureArray, normalsArray
}

func cleanUpProgram(prog Program) {
	gl.UseProgram(0)
	gl.DetachShader(prog.programID, prog.vertexID)
	gl.DetachShader(prog.programID, prog.fragmentID)
	gl.DeleteShader(prog.vertexID)
	gl.DeleteShader(prog.fragmentID)
	gl.DeleteProgram(prog.programID)
}

func loadViewMatrix(c Camera, program Program) {
	matrix := createViewMatrix(c)
	gl.UniformMatrix4fv(program.uniformLocations["location_viewMatrix"], 1,false, &matrix[0])
}

func loadShineVariables(damper, reflectivity float32, program Program) {
	gl.Uniform1f(program.uniformLocations["location_shineDamper"], damper)
	gl.Uniform1f(program.uniformLocations["location_reflectivity"], reflectivity)
}

func loadLight(light Light, program Program) {
	gl.Uniform3f(program.uniformLocations["location_lightColour"], light.colour.X(), light.colour.Y(), light.colour.Z())
	gl.Uniform3f(program.uniformLocations["location_lightPosition"], light.position.X(), light.position.Y(), light.position.Z())
}

func getAllUniformLocations(program Program) {
	program.uniformLocations["location_transformationMatrix"] = gl.GetUniformLocation(program.programID, gl.Str("transformationMatrix\x00"))
	program.uniformLocations["location_projectionMatrix"] = gl.GetUniformLocation(program.programID, gl.Str("projectionMatrix\x00"))
	program.uniformLocations["location_viewMatrix"] = gl.GetUniformLocation(program.programID, gl.Str("viewMatrix\x00"))
	program.uniformLocations["location_lightPosition"] = gl.GetUniformLocation(program.programID, gl.Str("lightPosition\x00"))
	program.uniformLocations["location_lightColour"] = gl.GetUniformLocation(program.programID, gl.Str("lightColour\x00"))
	program.uniformLocations["location_shineDamper"] = gl.GetUniformLocation(program.programID, gl.Str("shineDamper\x00"))
	program.uniformLocations["location_reflectivity"] = gl.GetUniformLocation(program.programID, gl.Str("reflectivity\x00"))
}

func bindAttributes(program Program) {
	gl.BindAttribLocation(program.programID, 0, gl.Str("position\x00"))
	gl.BindAttribLocation(program.programID, 1, gl.Str("textureCoords\x00"))
	gl.BindAttribLocation(program.programID, 2, gl.Str("normal\x00"))
}

func newProgram(vert, frag string, bindAttributes, getAllUniformLocations func(Program)) (Program, error) {
	vertexID, err := loadShader(vert, gl.VERTEX_SHADER)
	if err != nil {
		return Program{}, err
	}

	fragID, err := loadShader(frag, gl.FRAGMENT_SHADER)
	if err != nil {
		return Program{}, err
	}

	programID := gl.CreateProgram()
	gl.AttachShader(programID, vertexID)
	gl.AttachShader(programID, fragID)

	program := Program{programID,  vertexID, fragID, make(map[string]int32)}

	bindAttributes(program)

	gl.LinkProgram(programID)
	gl.ValidateProgram(programID)

	getAllUniformLocations(program)

	return program, nil
}

func loadShader(filename string, shaderType uint32) (uint32, error) {
	file, err := ioutil.ReadFile(filename)
	check(err)
	shader := gl.CreateShader(shaderType)

	csources, free := gl.Strs(string(file))
	gl.ShaderSource(shader, 1, csources, nil)
	free()
	gl.CompileShader(shader)

	var status int32
	gl.GetShaderiv(shader, gl.COMPILE_STATUS, &status)
	if status == gl.FALSE {
		var logLength int32
		gl.GetShaderiv(shader, gl.INFO_LOG_LENGTH, &logLength)

		log := strings.Repeat("\x00", int(logLength+1))
		gl.GetShaderInfoLog(shader, logLength, nil, gl.Str(log))

		return 0, fmt.Errorf("failed to compile %v: %v", file, log)
	}

	return shader, nil
}

func createTransformationMatrix(translation mgl32.Vec3, rx, ry, rz, scale float32) mgl32.Mat4 {
	matrix := mgl32.Ident4()
	matrix = matrix.Mul4(mgl32.Translate3D(translation.X(), translation.Y(), translation.Z()))
	matrix = matrix.Mul4(mgl32.HomogRotate3DX(toRadian(rx)))
	matrix = matrix.Mul4(mgl32.HomogRotate3DY(toRadian(ry)))
	matrix = matrix.Mul4(mgl32.HomogRotate3DZ(toRadian(rz)))
	matrix = matrix.Mul4(mgl32.Scale3D(scale, scale, scale))
	return matrix
}

func createViewMatrix(c Camera) mgl32.Mat4 {
	matrix := mgl32.Ident4()
	matrix = matrix.Mul4(mgl32.HomogRotate3DX(toRadian(c.pitch)))
	matrix = matrix.Mul4(mgl32.HomogRotate3DY(toRadian(c.yaw)))
	matrix = matrix.Mul4(mgl32.Translate3D(-c.position.X(), -c.position.Y(), -c.position.Z()))
	return matrix
}
//DEBUGGING PURPOSES ONLY
var nbFrames int
var lastTime float64
func FPSCounter() {
	currentTime := glfw.GetTime()
	nbFrames++
	if currentTime - lastTime >= 1.0 {
		fmt.Printf("%f ms/frame\n", 1000.0/float64(nbFrames))
		nbFrames = 0
		lastTime += 1.0
	}
}

func makeEntity(objloc , textureLoc string, position mgl32.Vec3, shine, reflectivity float32) Entity{
	textureID, err := loadTexture(textureLoc)
	check(err)
	texturedModel := TexturedModel{loadOBJModel(objloc), ModelTexture{textureID, shine, reflectivity}}
	return Entity{texturedModel, position, 0,0,0,1}
}

func main() {
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}
	defer glfw.Terminate()

	window = initDisplay()
	defer window.Destroy()

	window.SetKeyCallback(onKey)

	// Initialize Glow
	if err := gl.Init(); err != nil {
		panic(err)
	}

	version := gl.GoStr(gl.GetString(gl.VERSION))
	fmt.Println("OpenGL version", version)

	program, err := newProgram("vertexShader.txt", "fragmentShader.txt", bindAttributes, getAllUniformLocations)
	check(err)
	gl.UseProgram(program.programID)
	defer cleanUpProgram(program)

	gl.Enable(gl.CULL_FACE)
	gl.CullFace(gl.BACK)
	createProjectionMatrix(program)

	entity := makeEntity("dragon.obj", "res/blue_color.png", mgl32.Vec3{0, 0, -15}, 10, 1)

	entity2 := makeEntity("stall.obj", "res/stallTexture.png", mgl32.Vec3{0, 0, -15}, 1, 0)

	light := Light{mgl32.Vec3{0,0,-10}, mgl32.Vec3{1,1,1}}

	camera := Camera{mgl32.Vec3{0, 0, 0} , 0, 0, 0}

	for !window.ShouldClose() {
		entity.increaseRotation(0, 1, 0)
		camera.move()

		processEntity(entity2)
		processEntity(entity)

		render(light, camera, program)

		glfw.PollEvents()
		window.SwapBuffers()
		FPSCounter()
	}
}
