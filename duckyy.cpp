
#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/constants.hpp>

#include <learnopengl/filesystem.h>
#include <learnopengl/shader_m.h>
#include <learnopengl/model.h>

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <random>
#include <map>
#include <filesystem>

#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include <ft2build.h>
#include FT_FREETYPE_H

static const unsigned int GW_SCR_WIDTH = 800;
static const unsigned int GW_SCR_HEIGHT = 600;

// ---------------- Maps ----------------
static std::vector<std::vector<std::string>> GW_MAPS = {
    {
    "###############",
    "#K....#....M..#",
    "#.###.#.#####.#",
    "#.#...#.....#.#",
    "#.#.#####.#.#.#",
    "#.#.....#...#.#",
    "#.#####.#.#.#.#",
    "#.....#.#.#...#",
    "###.#.#.#.###.#",
    "#P..#.#.......#",
    "#.###.#.#####.#",
    "#.....#.....#.#",
    "#.#####.###.#.#",
    "#.......#..G#.#",
    "###############"
    },
    {
    "#####################",
    "#P....#.............#",
    "#.#######.#####.###.#",
    "#...#...#.....#...#.#",
    "###.#.#.###.#.###.#.#",
    "#...#.#...#.#.....#.#",
    "#.###.###.#.#######.#",
    "#.#.....#.#.....#...#",
    "#.#.###.#.#####.#.#.#",
    "#...#...#...M...#.#.#",
    "#####.#######.###.#.#",
    "#.....#.....#.....#.#",
    "#.#####.###.##.####.#",
    "#.....#...#.....#..K#",
    "#.###.###.#####.#.###",
    "#...#.....#.....#..G#",
    "#####################"
    }
};

static std::vector<std::string> GW_MAP;
static std::vector<std::string> GW_MAP_ORIG;
static int GW_CUR_MAP = 0;
static int GW_GRID_W = 0, GW_GRID_H = 0;

static void gw_fixMapWidth() {
    GW_GRID_H = (int)GW_MAP.size();
    GW_GRID_W = (GW_GRID_H > 0 ? (int)GW_MAP[0].size() : 0);
    for (auto& r : GW_MAP) {
        if ((int)r.size() < GW_GRID_W) r += std::string(GW_GRID_W - (int)r.size(), '#');
        if ((int)r.size() > GW_GRID_W) r = r.substr(0, GW_GRID_W);
    }
}

// load map by index and set both GW_MAP and GW_MAP_ORIG
static void gw_setMap(int idx) {
    GW_CUR_MAP = (idx % (int)GW_MAPS.size() + (int)GW_MAPS.size()) % (int)GW_MAPS.size();
    GW_MAP_ORIG = GW_MAPS[GW_CUR_MAP];
    GW_MAP = GW_MAP_ORIG;
    gw_fixMapWidth();
}

static inline bool gw_wallAt(int x, int y) {
    if (x < 0 || x >= GW_GRID_W || y < 0 || y >= GW_GRID_H) return true;
    return GW_MAP[y][x] == '#';
}
static inline glm::vec2 gw_centerOf(const glm::ivec2& t) { return glm::vec2(t) + glm::vec2(0.5f); }

// ======================================================================
// Random helpers
// ======================================================================
static std::mt19937& gw_rng() {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    return gen;
}
static inline int gw_manhattan(const glm::ivec2& a, const glm::ivec2& b) {
    return std::abs(a.x - b.x) + std::abs(a.y - b.y);
}
static int gw_countInOrig(char ch) {
    int cnt = 0;
    for (auto& row : GW_MAP_ORIG)
        for (char c : row) if (c == ch) ++cnt;
    return cnt;
}
static std::vector<glm::ivec2> gw_collectEmpties() {
    std::vector<glm::ivec2> v;
    v.reserve(GW_GRID_W * GW_GRID_H);
    for (int y = 0; y < GW_GRID_H; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == '.') v.push_back({ x, y });
    return v;
}
template <typename Pred>
static bool gw_pickRandomPos(glm::ivec2& out, const std::vector<glm::ivec2>& pool, Pred ok) {
    std::vector<glm::ivec2> cand;
    cand.reserve(pool.size());
    for (auto& p : pool) if (ok(p)) cand.push_back(p);
    if (cand.empty()) return false;
    std::uniform_int_distribution<size_t> dist(0, cand.size() - 1);
    out = cand[dist(gw_rng())];
    return true;
}
static bool gw_placeRandomGun(const std::vector<glm::ivec2>& avoid = {}, int attempts = 512, int minDistAvoid = 1) {
    if (GW_GRID_W <= 0 || GW_GRID_H <= 0) return false;
    auto empties = gw_collectEmpties();
    if (empties.empty()) return false;
    std::uniform_int_distribution<size_t> dist(0, empties.size() - 1);
    for (int i = 0; i < attempts; ++i) {
        glm::ivec2 p = empties[dist(gw_rng())];
        bool ok = true;
        for (auto& a : avoid) {
            if (gw_manhattan(a, p) <= minDistAvoid) { ok = false; break; }
        }
        if (!ok) continue;
        GW_MAP[p.y][p.x] = 'K';
        std::cout << "Gun spawned at " << p.x << "," << p.y << "\n";
        return true;
    }
    return false;
}
static std::vector<glm::ivec2> gw_spawnGhostsRandom(const glm::ivec2& playerTile, int n,
    int minFromPlayer = 2, int minBetweenGhosts = 2) {
    std::vector<glm::ivec2> placed;
    if (n <= 0) return placed;
    auto empties = gw_collectEmpties();
    for (int i = 0; i < n; ++i) {
        glm::ivec2 pick;
        bool ok = gw_pickRandomPos(pick, empties, [&](const glm::ivec2& p) {
            if (gw_manhattan(p, playerTile) < minFromPlayer) return false;
            for (auto& g : placed) if (gw_manhattan(p, g) < minBetweenGhosts) return false;
            return true;
            });
        if (!ok) break;
        placed.push_back(pick);
    }
    return placed;
}
static void gw_spawnMapItemM_Random(const glm::ivec2& playerTile,
    const std::vector<glm::ivec2>& ghostTiles,
    int n, int minFromPlayer = 2, int minFromGhosts = 1) {
    for (int y = 0; y < GW_GRID_H; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == 'M') GW_MAP[y][x] = '.';
    if (n <= 0) return;
    auto empties = gw_collectEmpties();
    int placed = 0;
    while (placed < n) {
        glm::ivec2 pick;
        bool ok = gw_pickRandomPos(pick, empties, [&](const glm::ivec2& p) {
            if (gw_manhattan(p, playerTile) < minFromPlayer) return false;
            for (auto& g : ghostTiles) if (gw_manhattan(p, g) < minFromGhosts) return false;
            return true;
            });
        if (!ok) break;
        GW_MAP[pick.y][pick.x] = 'M';
        ++placed;
    }
}

// ---------------- helper: find any safe tile ('.' or 'C') ----------------
// returns true and sets out if found
static bool gw_findAnyEmpty(glm::ivec2& out) {
    for (int y = 0; y < GW_GRID_H; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == '.' || GW_MAP[y][x] == 'C') {
                out = { x, y };
                return true;
            }
    return false;
}

// ---------------- Minimal color shader (with fog) ----------------
static const char* GW_COLOR_VS = R"(#version 330 core
layout(location=0) in vec3 aPos;
layout(location=1) in vec3 aNormal;

uniform mat4 model, view, projection;

out vec3 N;
out vec3 Vpos;

void main() {
    mat3 Nmat = mat3(transpose(inverse(model)));
    N = normalize(Nmat * aNormal);

    vec4 worldPos = model * vec4(aPos, 1.0);
    vec4 viewPos  = view * worldPos;
    Vpos = viewPos.xyz;

    gl_Position = projection * viewPos;
})";

static const char* GW_COLOR_FS = R"(#version 330 core
in vec3 N;
in vec3 Vpos;

out vec4 FragColor;

uniform vec3 uColor;

const vec3  fogColor   = vec3(0.04, 0.05, 0.08);
const float fogDensity = 0.045;

void main() {
    vec3 L = normalize(vec3(0.8, 1.2, 0.7));
    float d = max(dot(normalize(N), L), 0.0);
    vec3 base = uColor * (0.25 + 0.75 * d);

    float dist = length(Vpos);
    float fog  = clamp(exp(-pow(fogDensity * dist, 2.0)), 0.0, 1.0);
    vec3 col   = mix(fogColor, base, fog);

    FragColor = vec4(col, 1.0);
})";

static GLuint gw_makeProgram(const char* vs, const char* fs) {
    auto comp = [&](GLenum t, const char* s) {
        GLuint id = glCreateShader(t);
        glShaderSource(id, 1, &s, nullptr); glCompileShader(id);
        GLint ok; glGetShaderiv(id, GL_COMPILE_STATUS, &ok);
        if (!ok) { char log[1024]; glGetShaderInfoLog(id, 1024, nullptr, log); std::cerr << log << "\n"; }
        return id;
        };
    GLuint v = comp(GL_VERTEX_SHADER, vs), f = comp(GL_FRAGMENT_SHADER, fs);
    GLuint p = glCreateProgram(); glAttachShader(p, v); glAttachShader(p, f); glLinkProgram(p);
    glDeleteShader(v); glDeleteShader(f); return p;
}

static GLuint gw_colorProg = 0, gw_cubeVAO = 0, gw_cubeVBO = 0;
static void gw_initCube() {
    if (gw_cubeVAO) return;
    const float v[] = {
      -0.5f,0,-0.5f,0,0,-1,  0.5f,0,-0.5f,0,0,-1,  0.5f,1,-0.5f,0,0,-1,
      -0.5f,0,-0.5f,0,0,-1,  0.5f,1,-0.5f,0,0,-1, -0.5f,1,-0.5f,0,0,-1,
      -0.5f,0,0.5f ,0,0, 1,  0.5f,0,0.5f ,0,0, 1,  0.5f,1,0.5f ,0,0, 1,
      -0.5f,0,0.5f ,0,0, 1,  0.5f,1,0.5f ,0,0, 1, -0.5f,1,0.5f ,0,0, 1,
      -0.5f,0,-0.5f,-1,0,0,  -0.5f,0,0.5f,-1,0,0,  -0.5f,1,0.5f,-1,0,0,
      -0.5f,0,-0.5f,-1,0,0,  -0.5f,1,0.5f,-1,0,0,  -0.5f,1,-0.5f,-1,0,0,
       0.5f,0,-0.5f, 1,0,0,   0.5f,0,0.5f, 1,0,0,   0.5f,1,0.5f, 1,0,0,
       0.5f,0,-0.5f, 1,0,0,   0.5f,1,0.5f, 1,0,0,   0.5f,1,-0.5f, 1,0,0,
      -0.5f,1,-0.5f,0,1,0,    0.5f,1,-0.5f,0,1,0,   0.5f,1,0.5f,0,1,0,
      -0.5f,1,-0.5f,0,1,0,    0.5f,1,0.5f,0,1,0,   -0.5f,1,0.5f,0,1,0,
      -0.5f,0,-0.5f,0,-1,0,   0.5f,0,-0.5f,0,-1,0,  0.5f,0,0.5f,0,-1,0,
      -0.5f,0,-0.5f,0,-1,0,   0.5f,0,0.5f,0,-1,0,  -0.5f,0,0.5f,0,-1,0,
    };
    glGenVertexArrays(1, &gw_cubeVAO); glGenBuffers(1, &gw_cubeVBO);
    glBindVertexArray(gw_cubeVAO);
    glBindBuffer(GL_ARRAY_BUFFER, gw_cubeVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(v), v, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0); glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float))); glEnableVertexAttribArray(1);
    glBindVertexArray(0);
}
static void gw_drawCube(const glm::mat4& V, const glm::mat4& P, const glm::vec3& pos,
    const glm::vec3& size, const glm::vec3& color, float yawDeg = 0.f) {
    glUseProgram(gw_colorProg);
    glm::mat4 M(1.f);
    M = glm::translate(M, pos);
    M = glm::rotate(M, glm::radians(yawDeg), glm::vec3(0, 1, 0));
    M = glm::scale(M, size);
    glUniformMatrix4fv(glGetUniformLocation(gw_colorProg, "model"), 1, GL_FALSE, glm::value_ptr(M));
    glUniformMatrix4fv(glGetUniformLocation(gw_colorProg, "view"), 1, GL_FALSE, glm::value_ptr(V));
    glUniformMatrix4fv(glGetUniformLocation(gw_colorProg, "projection"), 1, GL_FALSE, glm::value_ptr(P));
    glUniform3f(glGetUniformLocation(gw_colorProg, "uColor"), color.x, color.y, color.z);
    glBindVertexArray(gw_cubeVAO); glDrawArrays(GL_TRIANGLES, 0, 36); glBindVertexArray(0);
}

// ==== Sphere mesh (for bullets / effects / map item / coins) ====
static GLuint gw_sphereVAO = 0, gw_sphereVBO = 0, gw_sphereEBO = 0;
static GLsizei gw_sphereIndexCount = 0;

static void gw_initSphere(int stacks = 12, int slices = 18) {
    if (gw_sphereVAO) return;

    std::vector<float> verts;   // pos(3) + normal(3)
    std::vector<unsigned int> idx;
    verts.reserve((stacks + 1) * (slices + 1) * 6);

    for (int i = 0; i <= stacks; ++i) {
        float v = (float)i / stacks;
        float phi = v * glm::pi<float>();
        float cp = std::cos(phi), sp = std::sin(phi);

        for (int j = 0; j <= slices; ++j) {
            float u = (float)j / slices;
            float theta = u * glm::two_pi<float>();
            float ct = std::cos(theta), st = std::sin(theta);

            glm::vec3 n = { ct * sp, cp, st * sp };
            verts.insert(verts.end(), { n.x, n.y, n.z, n.x, n.y, n.z });
        }
    }

    for (int i = 0; i < stacks; ++i) {
        for (int j = 0; j < slices; ++j) {
            int row1 = i * (slices + 1);
            int row2 = (i + 1) * (slices + 1);
            int a = row1 + j;
            int b = row1 + j + 1;
            int c = row2 + j;
            int d = row2 + j + 1;
            idx.insert(idx.end(), { (unsigned)a,(unsigned)c,(unsigned)b, (unsigned)b,(unsigned)c,(unsigned)d });
        }
    }
    gw_sphereIndexCount = (GLsizei)idx.size();

    glGenVertexArrays(1, &gw_sphereVAO);
    glGenBuffers(1, &gw_sphereVBO);
    glGenBuffers(1, &gw_sphereEBO);

    glBindVertexArray(gw_sphereVAO);
    glBindBuffer(GL_ARRAY_BUFFER, gw_sphereVBO);
    glBufferData(GL_ARRAY_BUFFER, verts.size() * sizeof(float), verts.data(), GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, gw_sphereEBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx.size() * sizeof(unsigned int), idx.data(), GL_STATIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);

    glBindVertexArray(0);
}

static void gw_drawSphere(const glm::mat4& V, const glm::mat4& P,
    const glm::vec3& center, float radius,
    const glm::vec3& color) {
    glUseProgram(gw_colorProg);
    glm::mat4 M(1.0f);
    M = glm::translate(M, center);
    M = glm::scale(M, glm::vec3(radius));

    glUniformMatrix4fv(glGetUniformLocation(gw_colorProg, "model"), 1, GL_FALSE, glm::value_ptr(M));
    glUniformMatrix4fv(glGetUniformLocation(gw_colorProg, "view"), 1, GL_FALSE, glm::value_ptr(V));
    glUniformMatrix4fv(glGetUniformLocation(gw_colorProg, "projection"), 1, GL_FALSE, glm::value_ptr(P));
    glUniform3f(glGetUniformLocation(gw_colorProg, "uColor"), color.x, color.y, color.z);

    glBindVertexArray(gw_sphereVAO);
    glDrawElements(GL_TRIANGLES, gw_sphereIndexCount, GL_UNSIGNED_INT, 0);
    glBindVertexArray(0);
}

// ---------------- Coins (global) ----------------
static int  g_coinCollected = 0;
static int  g_coinTotal = 0;
static int  g_requiredCoins = 100; // coins required to win (global target across maps)
static bool g_titleDirty = false;

// Win state info
static int g_winCoins = 0;

// last outcome flag: true = lose (caught), false = win (collected required coins)
static bool g_lastWasLose = false;
static bool g_killedAnyGhost = false; // set true when player kills any ghost

static void gw_updateTitle(GLFWwindow* win) {
    std::string t = "Grid Walk 3D — Coins: " +
        std::to_string(g_coinCollected) + "/" + std::to_string(g_coinTotal);
    glfwSetWindowTitle(win, t.c_str());
    g_titleDirty = false;
}

// ปรับ: วางเหรียญที่ทุกช่อง '.' (เหมือนเดิม) และนับจำนวนรวม
static void gw_fillCoinsOnSomeFloorsAndCount() {
    // g_coinCollected persists across maps; g_coinTotal updated per-map
    g_coinTotal = 0;
    // clear previous coins (keep map walls, K, M)
    for (int y = 0; y < GW_GRID_H; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == 'C')
                GW_MAP[y][x] = '.';

    // Place coin on every '.' tile
    for (int y = 0; y < GW_GRID_H; ++y) {
        for (int x = 0; x < GW_GRID_W; ++x) {
            if (GW_MAP[y][x] == '.') {
                GW_MAP[y][x] = 'C';
            }
        }
    }

    // count total
    for (int y = 0; y < GW_GRID_H; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == 'C') ++g_coinTotal;

    // set required coins to win: require collecting 100 coins (clamped to total)
    g_requiredCoins = std::min(std::max(1, g_coinTotal), 100);
    // You want 100 required for win; if map has fewer coins, require the map total.
    g_titleDirty = true;
}

// ---------------- Entities ----------------
struct GWMoveCtrl {
    bool        moving = false;
    glm::ivec2  dir{ 0,0 };
    glm::ivec2  queued{ 0,0 };
    glm::vec2   target{ 0 };
};
struct GWEntity {
    glm::vec2 pos{ 0 };
    float     yaw = 0.f;
    GWMoveCtrl ctrl;

    // visual elevation (no jumping now, keep zero)
    float     elev = 0.0f;    // elevation above tile (visual)
    bool      onGround = true;
};
struct GWBullet {
    glm::vec2 pos;
    glm::vec2 dir;
    float     life = 1.5f;
    bool      alive = true;
};



// =============== BONUS MAP STATE ===============
struct GWMapState {
    std::vector<std::string> map;
    std::vector<std::string> mapOrig;
    GWEntity                 player;
    std::vector<GWEntity>    ghosts;
    std::vector<GWBullet>    bullets;
    bool   hasGun = false;
    float  fireCooldown = 0.0f;
    float  savedGunRespawnTimer = 0.0f;
    int    coinCollected = 0;
    int    coinTotal = 0;
    int    curMapIndex = 0;
};

static bool       g_inBonus = false;
static float      g_bonusTimer = 0.0f;
static const float BONUS_DURATION = 20.0f; 
static GWMapState g_savedMainState;

// Speeds
static const float GW_STEP_SPEED_PLAYER = 6.0f;
static const float GW_STEP_SPEED_ENEMY = 3.0f;
static const float GW_BULLET_SPEED = 12.0f;
static const float GW_FIRE_COOLDOWN = 0.25f;

// Gun respawn timing
static const float GUN_RESPAWN_INTERVAL = 8.0f; // วินาที
static float gunRespawnTimer = GUN_RESPAWN_INTERVAL;

// Jump input timing
// Set extremely large so jump never triggers (effectively disables jump)
static const float JUMP_HOLD_THRESHOLD = 1e9f; // hold time (s) to trigger jump instead of shoot

// Mouse wheel zoom
static float* GW_camDistPtr = nullptr;
static void gw_scroll_callback(GLFWwindow*, double, double yoffset) {
    if (!GW_camDistPtr) return;
    float& d = *GW_camDistPtr;
    d -= (float)yoffset * 0.8f;
    d = std::max(2.0f, std::min(30.0f, d));
}

// Input (tile direction)
static glm::ivec2 gw_readInput(GLFWwindow* w) {
    if (glfwGetKey(w, GLFW_KEY_LEFT) == GLFW_PRESS || glfwGetKey(w, GLFW_KEY_A) == GLFW_PRESS) return { -1,0 };
    if (glfwGetKey(w, GLFW_KEY_RIGHT) == GLFW_PRESS || glfwGetKey(w, GLFW_KEY_D) == GLFW_PRESS) return { 1,0 };
    if (glfwGetKey(w, GLFW_KEY_UP) == GLFW_PRESS || glfwGetKey(w, GLFW_KEY_W) == GLFW_PRESS) return { 0,-1 };
    if (glfwGetKey(w, GLFW_KEY_DOWN) == GLFW_PRESS || glfwGetKey(w, GLFW_KEY_S) == GLFW_PRESS) return { 0, 1 };
    return { 0,0 };
}

// Enemy greedy steering
static glm::ivec2 gw_chooseDirChase(const glm::ivec2& fromTile, const glm::ivec2& curDir, const glm::ivec2& playerTile) {
    std::vector<glm::ivec2> dirs = { {1,0},{-1,0},{0,1},{0,-1} };
    glm::ivec2 best = curDir; int bestScore = 1e9;
    for (auto d : dirs) {
        if (d == -curDir) continue;
        glm::ivec2 nt = fromTile + d;
        if (gw_wallAt(nt.x, nt.y)) continue;
        int s = std::abs(playerTile.x - nt.x) + std::abs(playerTile.y - nt.y);
        if (s < bestScore) { bestScore = s; best = d; }
    }
    if (bestScore == 1e9) {
        glm::ivec2 rev = -curDir;
        if (!gw_wallAt(fromTile.x + rev.x, fromTile.y + rev.y)) return rev;
        return { 0,0 };
    }
    return best;
}

// Draw a Model with transforms
static void gw_drawModel(Shader& sh, Model& mdl, const glm::mat4& V, const glm::mat4& P,
    const glm::vec3& pos, const glm::vec3& scl = glm::vec3(1.0f),
    float yawDeg = 0.f, float pitchDeg = 0.f, float rollDeg = 0.f) {
    sh.use();
    glm::mat4 M(1.f);
    M = glm::translate(M, pos);
    if (yawDeg != 0.f)   M = glm::rotate(M, glm::radians(yawDeg), glm::vec3(0, 1, 0));
    if (pitchDeg != 0.f) M = glm::rotate(M, glm::radians(pitchDeg), glm::vec3(1, 0, 0));
    if (rollDeg != 0.f)  M = glm::rotate(M, glm::radians(rollDeg), glm::vec3(0, 0, 1));
    M = glm::scale(M, scl);
    sh.setMat4("model", M);
    sh.setMat4("view", V);
    sh.setMat4("projection", P);
    mdl.Draw(sh);
}

// ---------- Map load / Reset ----------
static void gw_captureState(
    GWMapState& out,
    const GWEntity& player,
    const std::vector<GWEntity>& ghosts,
    const std::vector<GWBullet>& bullets,
    bool hasGun, float fireCooldown
) {
    out.map = GW_MAP;
    out.mapOrig = GW_MAP_ORIG;
    out.player = player;
    out.ghosts = ghosts;
    out.bullets = bullets;
    out.hasGun = hasGun;
    out.fireCooldown = fireCooldown;
    out.savedGunRespawnTimer = gunRespawnTimer;
    out.coinCollected = g_coinCollected;
    out.coinTotal = g_coinTotal;
    out.curMapIndex = GW_CUR_MAP;
}

// -------------- restore with sanitization --------------
static void gw_restoreState(
    const GWMapState& s,
    GWEntity& player,
    std::vector<GWEntity>& ghosts,
    std::vector<GWBullet>& bullets,
    bool& hasGun, float& fireCooldown
) {
    // restore map
    GW_MAP = s.map;
    GW_MAP_ORIG = s.mapOrig;
    GW_CUR_MAP = s.curMapIndex;
    gw_fixMapWidth();

    // restore raw entities
    player = s.player;
    ghosts = s.ghosts;
    bullets = s.bullets;
    hasGun = s.hasGun;
    fireCooldown = s.fireCooldown;
    gunRespawnTimer = s.savedGunRespawnTimer;
    g_coinCollected = s.coinCollected;
    g_coinTotal = s.coinTotal;

    // --- sanitize player position & movement ---
    {
        glm::ivec2 pt = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
        if (pt.x < 0 || pt.x >= GW_GRID_W || pt.y < 0 || pt.y >= GW_GRID_H || gw_wallAt(pt.x, pt.y)) {
            // find any empty/cell to put player (prefer 'P' if present)
            bool placed = false;
            for (int y = 0; y < GW_GRID_H && !placed; ++y)
                for (int x = 0; x < GW_GRID_W; ++x) {
                    if (GW_MAP[y][x] == 'P' || GW_MAP[y][x] == '.' || GW_MAP[y][x] == 'C') {
                        player.pos = { x + 0.5f, y + 0.5f };
                        placed = true;
                        break;
                    }
                }
            if (!placed) {
                // fallback to 0.5,0.5
                player.pos = { 0.5f, 0.5f };
            }
        }
        // snap to center of tile and clear movement state
        glm::ivec2 snapT = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
        player.pos = gw_centerOf(snapT);
        player.ctrl.moving = false;
        player.ctrl.queued = { 0, 0 };
        player.ctrl.target = player.pos;

        // reset vertical state (no jump)
        player.elev = 0.0f;
        player.onGround = true;
    }

    // --- sanitize ghosts: snap to tile centers and stop movement ---
    for (auto& g : ghosts) {
        glm::ivec2 gt = { (int)std::floor(g.pos.x), (int)std::floor(g.pos.y) };
        if (gt.x < 0 || gt.x >= GW_GRID_W || gt.y < 0 || gt.y >= GW_GRID_H || gw_wallAt(gt.x, gt.y)) {
            // find any empty for this ghost (simple strategy: find first empty not equal player tile)
            glm::ivec2 found;
            bool ok = false;
            for (int y = 0; y < GW_GRID_H && !ok; ++y)
                for (int x = 0; x < GW_GRID_W; ++x) {
                    if (GW_MAP[y][x] == '.' || GW_MAP[y][x] == 'C') {
                        if ((x != (int)std::floor(player.pos.x)) || (y != (int)std::floor(player.pos.y))) {
                            found = { x, y }; ok = true; break;
                        }
                    }
                }
            if (!ok) {
                found = { std::max(0, std::min(GW_GRID_W - 1, gt.x)), std::max(0, std::min(GW_GRID_H - 1, gt.y)) };
            }
            g.pos = gw_centerOf(found);
        }
        else {
            g.pos = gw_centerOf(gt);
        }
        g.ctrl.moving = false;
        g.ctrl.queued = { 0, 0 };
        g.ctrl.target = g.pos;
    }

    g_titleDirty = true;
}

static void gw_resetGame(
    GWEntity& player,
    std::vector<GWEntity>& ghosts,
    std::vector<GWBullet>& bullets,
    bool& hasGun, float& fireCooldown
) {
    // คืนแมพจากต้นฉบับของแมพปัจจุบัน
    GW_MAP = GW_MAP_ORIG;
    gw_fixMapWidth();

    ghosts.clear();
    bullets.clear();
    hasGun = false;
    g_killedAnyGhost = false;
    fireCooldown = 0.0f;
    gunRespawnTimer = GUN_RESPAWN_INTERVAL;
    player = GWEntity{}; // reset movement/yaw

    // หา P แล้วลบทิ้งจากแผนที่ใช้งาน
    bool foundP = false;
    for (int y = 0; y < GW_GRID_H; ++y) {
        for (int x = 0; x < GW_GRID_W; ++x) {
            if (GW_MAP[y][x] == 'P') {
                player.pos = { x + 0.5f, y + 0.5f };
                GW_MAP[y][x] = '.';
                foundP = true;
            }
        }
    }
    // If no P found, fallback to first empty cell
    if (!foundP) {
        for (int y = 0; y < GW_GRID_H && !foundP; ++y)
            for (int x = 0; x < GW_GRID_W; ++x)
                if (GW_MAP[y][x] == '.') {
                    player.pos = { x + 0.5f, y + 0.5f };
                    foundP = true;
                    break;
                }
    }

    // เคลียร์ G/M เดิมทิ้งให้หมด
    for (int y = 0; y < GW_GRID_H; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == 'G' || GW_MAP[y][x] == 'M')
                GW_MAP[y][x] = '.';

    // จำนวนเป้าหมายในการสุ่ม: อิงจากจำนวนในแมพต้นฉบับ (ขั้นต่ำอย่างละ 1)
    int nGhosts = std::max(1, gw_countInOrig('G'));
    int nMs = std::max(1, gw_countInOrig('M'));

    // สุ่มเกิดผี (entity)
    glm::ivec2 pTile = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
    auto ghostTiles = gw_spawnGhostsRandom(pTile, nGhosts, /*minFromPlayer=*/2, /*minBetweenGhosts=*/2);
    if (ghostTiles.empty()) { // fallback ถ้าแมพแคบมาก
        ghostTiles.push_back({ std::max(1, GW_GRID_W - 2), std::max(1, GW_GRID_H - 2) });
    }
    for (auto& gt : ghostTiles) {
        GWEntity g; g.pos = { gt.x + 0.5f, gt.y + 0.5f };
        // ensure ghosts not moving at start
        g.ctrl.moving = false;
        g.ctrl.queued = { 0,0 };
        g.ctrl.target = g.pos;
        ghosts.push_back(g);
    }

    // ถ้าไม่มี 'K' ในแมพ ให้สุ่มวาง 1 กระบอก
    bool foundK = false;
    for (int y = 0; y < GW_GRID_H && !foundK; ++y)
        for (int x = 0; x < GW_GRID_W; ++x)
            if (GW_MAP[y][x] == 'K') { foundK = true; break; }
    if (!foundK) {
        std::vector<glm::ivec2> avoid;
        avoid.push_back(pTile);
        for (auto& g : ghostTiles) avoid.push_back(g);
        gw_placeRandomGun(avoid, 512, 1);
    }

    // สุ่มวาง M (เขียนลงแมพ)
    gw_spawnMapItemM_Random(pTile, ghostTiles, nMs, /*minFromPlayer=*/2, /*minFromGhosts=*/1);

    // ปูเหรียญทุกจุด (วางหลัง K/M)
    gw_fillCoinsOnSomeFloorsAndCount();

    // ensure player's ctrl is cleared
    player.ctrl.moving = false;
    player.ctrl.queued = { 0,0 };
    player.ctrl.target = player.pos;

    // reset vertical state (no jump)
    player.elev = 0.0f;
    player.onGround = true;
}

// เข้าแมพโบนัสเก็บเหรียญ (ไม่มีศัตรู/ปืน)
static void gw_enterBonusMap(
    int bonusIndex,
    GWEntity& player,
    std::vector<GWEntity>& ghosts,
    std::vector<GWBullet>& bullets,
    bool& hasGun, float& fireCooldown
) {
    // โหลดแมพโบนัส
    gw_setMap(bonusIndex);

    ghosts.clear();
    bullets.clear();
    hasGun = false;
    g_killedAnyGhost = false;
    fireCooldown = 0.0f;
    gunRespawnTimer = GUN_RESPAWN_INTERVAL;


    bool foundP = false;
    for (int y = 0; y < GW_GRID_H; ++y) {
        for (int x = 0; x < GW_GRID_W; ++x) {
            char& c = GW_MAP[y][x];
            if (c == 'P') {
                player.pos = { x + 0.5f, y + 0.5f };
                c = '.';
                foundP = true;
            }
        }
    }

    if (!foundP) {
        for (int y = 0; y < GW_GRID_H && !foundP; ++y)
            for (int x = 0; x < GW_GRID_W; ++x)
                if (GW_MAP[y][x] == '.') {
                    player.pos = { x + 0.5f, y + 0.5f };
                    foundP = true;
                    break;
                }
    }

    for (int y = 0; y < GW_GRID_H; ++y) {
        for (int x = 0; x < GW_GRID_W; ++x) {
            char& c = GW_MAP[y][x];
            if (c == 'G' || c == 'K' || c == 'M') {
                c = '.';
            }
        }
    }

    gw_fillCoinsOnSomeFloorsAndCount();
    g_titleDirty = true;


    player.ctrl.moving = false;
    player.ctrl.queued = { 0,0 };
    player.ctrl.target = player.pos;

    // reset vertical state (no jump)
    player.elev = 0.0f;
    player.onGround = true;
}

// ===================== SKYBOX (ADD-ONLY) =====================
static GLuint skyboxVAO = 0, skyboxVBO = 0;
static unsigned int cubemapTexture = 0;
static Shader* skyboxShaderPtr = nullptr;

static void GW_InitSkyboxCube() {
    if (skyboxVAO) return;
    const float v[] = {
        // -Z
        -1,  1, -1,  -1, -1, -1,   1, -1, -1,   1, -1, -1,   1,  1, -1,  -1,  1, -1,
        // -X
        -1, -1,  1,  -1, -1, -1,  -1,  1, -1,  -1,  1, -1,  -1,  1,  1,  -1, -1,  1,
        // +X
         1, -1, -1,   1, -1,  1,   1,  1,  1,   1,  1,  1,   1,  1, -1,   1, -1, -1,
         // +Z
         -1, -1,  1,  -1,  1,  1,   1,  1,  1,   1,  1,  1,   1, -1,  1,  -1, -1,  1,
         // +Y
         -1,  1, -1,   1,  1, -1,   1,  1,  1,   1,  1,  1,  -1,  1,  1,  -1,  1, -1,
         // -Y
         -1, -1, -1,  -1, -1,  1,   1, -1, -1,   1, -1, -1,  -1, -1,  1,   1, -1,  1
    };
    glGenVertexArrays(1, &skyboxVAO);
    glGenBuffers(1, &skyboxVBO);
    glBindVertexArray(skyboxVAO);
    glBindBuffer(GL_ARRAY_BUFFER, skyboxVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(v), v, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);
    glBindVertexArray(0);
}

static GLuint loadCubemap(const std::vector<std::string>& faces) {
    GLuint texID; glGenTextures(1, &texID);
    glBindTexture(GL_TEXTURE_CUBE_MAP, texID);

    stbi_set_flip_vertically_on_load(false);

    int w, h, nc;
    for (unsigned int i = 0; i < faces.size(); ++i) {
        unsigned char* data = stbi_load(faces[i].c_str(), &w, &h, &nc, 0);
        if (data) {
            GLenum fmt = (nc == 4 ? GL_RGBA : GL_RGB);
            glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, fmt, w, h, 0, fmt, GL_UNSIGNED_BYTE, data);
            stbi_image_free(data);
        }
        else {
            std::cerr << "Failed to load cubemap face: " << faces[i] << "\n";
        }
    }
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    return texID;
}
// ============================================================

// ===================== Minimal 2D UI (START MENU + WIN) =====================
enum GWGameState { MENU, PLAYING, WON };
static GWGameState g_state = MENU;

static const char* GW_UI_VS = R"(#version 330 core
layout(location=0) in vec2 aPos;
uniform mat4 uMVP;
void main(){
  gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
})";

static const char* GW_UI_FS = R"(#version 330 core
out vec4 FragColor;
uniform vec3 uColor;
void main(){
  FragColor = vec4(uColor, 1.0);
})";

static GLuint gw_uiProg = 0, gw_uiVAO = 0, gw_uiVBO = 0, gw_uiEBO = 0;

static void gw_initUI() {
    if (gw_uiProg) return;
    auto comp = [&](GLenum t, const char* s) {
        GLuint id = glCreateShader(t);
        glShaderSource(id, 1, &s, nullptr); glCompileShader(id);
        GLint ok; glGetShaderiv(id, GL_COMPILE_STATUS, &ok);
        if (!ok) { char log[1024]; glGetShaderInfoLog(id, 1024, nullptr, log); std::cerr << log << "\n"; }
        return id;
        };
    GLuint v = comp(GL_VERTEX_SHADER, GW_UI_VS);
    GLuint f = comp(GL_FRAGMENT_SHADER, GW_UI_FS);
    gw_uiProg = glCreateProgram(); glAttachShader(gw_uiProg, v); glAttachShader(gw_uiProg, f); glLinkProgram(gw_uiProg);
    glDeleteShader(v); glDeleteShader(f);

    glGenVertexArrays(1, &gw_uiVAO);
    glGenBuffers(1, &gw_uiVBO);
    glGenBuffers(1, &gw_uiEBO);

    glBindVertexArray(gw_uiVAO);
    glBindBuffer(GL_ARRAY_BUFFER, gw_uiVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 8, nullptr, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);

    unsigned int idx[6] = { 0,1,2, 2,3,0 };
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, gw_uiEBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(idx), idx, GL_STATIC_DRAW);

    glBindVertexArray(0);
}

static void gw_drawUIRect(float x, float y, float w, float h,
    const glm::mat4& Ortho,
    const glm::vec3& color) {
    float verts[8] = {
        x,     y,
        x + w,   y,
        x + w,   y + h,
        x,     y + h
    };
    glUseProgram(gw_uiProg);
    glUniformMatrix4fv(glGetUniformLocation(gw_uiProg, "uMVP"), 1, GL_FALSE, glm::value_ptr(Ortho));
    glUniform3f(glGetUniformLocation(gw_uiProg, "uColor"), color.x, color.y, color.z);

    glBindVertexArray(gw_uiVAO);
    glBindBuffer(GL_ARRAY_BUFFER, gw_uiVBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(verts), verts);
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
    glBindVertexArray(0);
}

static bool gw_pointInRect(double mx, double my, float x, float y, float w, float h) {
    return (mx >= x && mx <= x + w && my >= y && my <= y + h);
}

// ============================================================

// --------------- Map switching helper (safe) ----------------
static void gw_changeMapAndReset(int delta,
    GWEntity& player,
    std::vector<GWEntity>& ghosts,
    std::vector<GWBullet>& bullets,
    bool& hasGun, float& fireCooldown)
{
    int newIndex = (GW_CUR_MAP + delta) % (int)GW_MAPS.size();
    if (newIndex < 0) newIndex += (int)GW_MAPS.size();
    gw_setMap(newIndex);
    g_killedAnyGhost = false;
    gw_resetGame(player, ghosts, bullets, hasGun, fireCooldown);
    g_titleDirty = true;
}

// ===================== FreeType Text Rendering (Added) =====================

struct Character {
    GLuint TextureID;
    glm::ivec2 Size;
    glm::ivec2 Bearing;
    GLuint Advance;
};
static std::map<GLchar, Character> Characters;
static GLuint textVAO = 0, textVBO = 0;
static GLuint textProg = 0;

static const char* TEXT_VS = R"(#version 330 core
layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
out vec2 TexCoords;
uniform mat4 projection;
void main() {
    gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
    TexCoords = vertex.zw;
})";

static const char* TEXT_FS = R"(#version 330 core
in vec2 TexCoords;
out vec4 color;
uniform sampler2D text;
uniform vec3 textColor;
void main() {
    float alpha = texture(text, TexCoords).r;
    color = vec4(textColor, alpha);
})";

static bool InitTextFont(const std::string& fontPath, int pixelHeight = 48) {
    FT_Library ft;
    if (FT_Init_FreeType(&ft)) {
        std::cerr << "ERROR::FREETYPE: Could not init FreeType Library\n";
        return false;
    }
    FT_Face face;
    if (FT_New_Face(ft, fontPath.c_str(), 0, &face)) {
        std::cerr << "ERROR::FREETYPE: Failed to load font: " << fontPath << "\n";
        FT_Done_FreeType(ft);
        return false;
    }
    FT_Set_Pixel_Sizes(face, 0, pixelHeight);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    Characters.clear();
    for (unsigned char c = 0; c < 128; ++c) {
        if (FT_Load_Char(face, c, FT_LOAD_RENDER)) {
            std::cerr << "WARN: Failed to load Glyph for char code " << (int)c << "\n";
            continue;
        }
        GLuint tex;
        glGenTextures(1, &tex);
        glBindTexture(GL_TEXTURE_2D, tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RED,
            face->glyph->bitmap.width, face->glyph->bitmap.rows,
            0, GL_RED, GL_UNSIGNED_BYTE, face->glyph->bitmap.buffer);

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        Character ch;
        ch.TextureID = tex;
        ch.Size = { (int)face->glyph->bitmap.width, (int)face->glyph->bitmap.rows };
        ch.Bearing = { (int)face->glyph->bitmap_left, (int)face->glyph->bitmap_top };
        ch.Advance = (GLuint)face->glyph->advance.x;
        Characters.insert(std::pair<GLchar, Character>((GLchar)c, ch));
    }
    glBindTexture(GL_TEXTURE_2D, 0);
    FT_Done_Face(face);
    FT_Done_FreeType(ft);

    glGenVertexArrays(1, &textVAO);
    glGenBuffers(1, &textVBO);
    glBindVertexArray(textVAO);
    glBindBuffer(GL_ARRAY_BUFFER, textVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 6 * 4, nullptr, GL_DYNAMIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    return true;
}

static void RenderTextProg(GLuint prog, std::string text, float x, float y, float scale, glm::vec3 color) {
    glUseProgram(prog);
    // set textColor (safe)
    GLint loc = glGetUniformLocation(prog, "textColor");
    if (loc >= 0) glUniform3f(loc, color.x, color.y, color.z);
    // ensure sampler is 0
    GLint locTex = glGetUniformLocation(prog, "text");
    if (locTex >= 0) glUniform1i(locTex, 0);

    glActiveTexture(GL_TEXTURE0);
    glBindVertexArray(textVAO);

    for (auto it = text.begin(); it != text.end(); ++it) {
        GLchar c = *it;
        auto found = Characters.find(c);
        if (found == Characters.end()) continue;
        Character ch = found->second;

        float xpos = x + ch.Bearing.x * scale;
        float ypos = y - (ch.Size.y - ch.Bearing.y) * scale;

        float w = ch.Size.x * scale;
        float h = ch.Size.y * scale;
        float vertices[6][4] = {
            { xpos,     ypos + h,   0.0f, 0.0f },
            { xpos,     ypos,       0.0f, 1.0f },
            { xpos + w, ypos,       1.0f, 1.0f },

            { xpos,     ypos + h,   0.0f, 0.0f },
            { xpos + w, ypos,       1.0f, 1.0f },
            { xpos + w, ypos + h,   1.0f, 0.0f }
        };
        glBindTexture(GL_TEXTURE_2D, ch.TextureID);
        glBindBuffer(GL_ARRAY_BUFFER, textVBO);
        glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(vertices), vertices);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        x += (ch.Advance >> 6) * scale;
    }
    glBindVertexArray(0);
    glBindTexture(GL_TEXTURE_2D, 0);
}

// Utility: process tile pickups when player lands on tile (or steps onto tile)
static void gw_processTilePickupAt(const glm::ivec2& pt, GWEntity& player, std::vector<GWEntity>& ghosts, std::vector<GWBullet>& bullets, bool& hasGun, float& fireCooldown, GLFWwindow* win) {
    if (pt.x < 0 || pt.x >= GW_GRID_W || pt.y < 0 || pt.y >= GW_GRID_H) return;
    char& tile = GW_MAP[pt.y][pt.x];
    if (tile == 'K') {
        hasGun = true;
        tile = '.';
        std::cout << "Picked up gun!\n";
        gunRespawnTimer = GUN_RESPAWN_INTERVAL;
        g_titleDirty = true;
    }
    else if (tile == 'M') {
        if (!g_inBonus) {
            std::cout << "Enter bonus map for 20 seconds!\n";
            tile = '.';
            gw_captureState(g_savedMainState, player, ghosts, bullets, hasGun, fireCooldown);
            g_inBonus = true;
            g_bonusTimer = BONUS_DURATION;
            int bonusIndex = 1;
            g_killedAnyGhost = false;
            gw_enterBonusMap(bonusIndex, player, ghosts, bullets, hasGun, fireCooldown);
        }
    }
    else if (tile == 'C') {
        tile = '.';
        ++g_coinCollected;
        // NEW: win immediately when reaching required coins
        if (!g_inBonus && g_coinCollected >= g_requiredCoins) {
            g_lastWasLose = false;
            g_state = WON;
            g_winCoins = g_coinCollected;
            bullets.clear();
            g_titleDirty = true;
        }
        g_titleDirty = true;
        // Note: auto-win removed. Now winning requires both collecting enough coins AND having killed at least one ghost and no ghosts left.
    }
}

// ===================== MAIN =====================
int main() {
    // โหลดแมพแรกเข้าระบบ
    gw_setMap(0);

    // ตัวแปรสถานะหลัก
    GWEntity player;
    std::vector<GWEntity> ghosts;
    bool hasGun = false; float fireCooldown = 0.0f; std::vector<GWBullet> bullets;

    // รีเกมครั้งแรก
    g_killedAnyGhost = false;
    gw_resetGame(player, ghosts, bullets, hasGun, fireCooldown);

    // --- GL init ---
    if (!glfwInit()) { std::cerr << "GLFW init fail\n"; return -1; }
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
#ifdef __APPLE__
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
#endif
    GLFWwindow* win = glfwCreateWindow(GW_SCR_WIDTH, GW_SCR_HEIGHT, "Help Duckky  Click the button or press Enter to start", nullptr, nullptr);
    if (!win) { std::cerr << "GLFW window fail\n"; glfwTerminate(); return -1; }
    glfwMakeContextCurrent(win);
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) { std::cerr << "GLAD fail\n"; return -1; }

    // REQUIRED for text rendering: enable blending for glyph alpha mask
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    gw_colorProg = gw_makeProgram(GW_COLOR_VS, GW_COLOR_FS);
    gw_initCube();
    gw_initSphere();

    // Model shader + models (these paths must exist in resources)
    Shader modelShader("1.model_loading.vs", "1.model_loading.fs");
    Model duck("resources/objects/duck2/duck.obj");
    Model rock("resources/objects/rock/rock.obj");
    Model gun("resources/objects/gun/gun.obj");
    Model& playerModel = duck;
    Model& ghostModel = rock;
    Model& gunModel = gun;

    // ========== SKYBOX INIT ==========
    glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

    static Shader skyboxShader("1.skybox.vs", "1.skybox.fs");
    skyboxShader.use();
    skyboxShader.setInt("skybox", 0);
    skyboxShaderPtr = &skyboxShader;

    GW_InitSkyboxCube();

    std::vector<std::string> faces{
        ("resources/textures/blue/right.png"),
        ("resources/textures/blue/left.png"),
        ("resources/textures/blue/top.png"),
        ("resources/textures/blue/bottom.png"),
        ("resources/textures/blue/front.png"),
        ("resources/textures/blue/back.png")
    };
    cubemapTexture = loadCubemap(faces);

    // UI init + Title
    gw_initUI();
    glfwSetWindowTitle(win, "Grid Walk 3D — Click the button or press Enter to start");

    // --- Text shader + font init (ADDED) ---
    textProg = gw_makeProgram(TEXT_VS, TEXT_FS);
    glUseProgram(textProg);
    glm::mat4 textProj = glm::ortho(0.0f, (float)GW_SCR_WIDTH, 0.0f, (float)GW_SCR_HEIGHT);
    GLint locProj = glGetUniformLocation(textProg, "projection");
    if (locProj >= 0) glUniformMatrix4fv(locProj, 1, GL_FALSE, glm::value_ptr(textProj));
    GLint locTex = glGetUniformLocation(textProg, "text");
    if (locTex >= 0) glUniform1i(locTex, 0);

    // font file (search resources/fonts) — prefer FileSystem path, fallback to absolute you provided
    std::string fontPath = ("resources/fonts/Antonio-Regular.ttf");
    std::string alt = R"(C:\Users\User\Downloads\LearnOpenGL-edit\resources\fonts\Antonio-Regular.ttf)";
    if (!std::filesystem::exists(fontPath)) {
        if (std::filesystem::exists(alt)) {
            fontPath = alt;
        }
    }
    if (!std::filesystem::exists(fontPath)) {
        std::cerr << "ERROR: font file not found (tried): " << fontPath << " and " << alt << "\n";
    }
    else {
        std::cerr << "Using font: " << fontPath << "\n";
    }

    if (!InitTextFont(fontPath, 48)) {
        std::cerr << "Warning: text font init failed. Text won't render.\n";
    }

    // Camera (Top-only)
    float camPitch = -58.0f;
    float camDist = glm::length(glm::vec2(5.0f, 7.0f));
    float camYaw = 180.0f + player.yaw;
    const float CAM_PITCH_MIN = -89.0f, CAM_PITCH_MAX = -10.0f;

    double lastMX = 0.0, lastMY = 0.0; bool rotating = false, rmbPrimed = false;
    const float SENS_X = 0.15f, SENS_Y = 0.15f;

    GW_camDistPtr = &camDist;
    glfwSetScrollCallback(win, gw_scroll_callback);

    double last = glfwGetTime();
    while (!glfwWindowShouldClose(win)) {
        double now = glfwGetTime(); float dt = float(now - last); last = now;
        glfwPollEvents();

        // input: map change keys (N/B) while playing
        if (g_state == PLAYING) {
            if (glfwGetKey(win, GLFW_KEY_N) == GLFW_PRESS) {
                // go next map (safe change)
                gw_changeMapAndReset(1, player, ghosts, bullets, hasGun, fireCooldown);
            }
            if (glfwGetKey(win, GLFW_KEY_B) == GLFW_PRESS) {
                gw_changeMapAndReset(-1, player, ghosts, bullets, hasGun, fireCooldown);
            }
        }

        // ------ START MENU: fixed coords (convert to text bottom-left origin) ------
        // ------ START MENU: minimal 'HELP DUCKKY' + START button only ------
        if (g_state == MENU) {
            // 2D setup
            glViewport(0, 0, GW_SCR_WIDTH, GW_SCR_HEIGHT);
            glDisable(GL_DEPTH_TEST);
            glClearColor(0.02f, 0.03f, 0.06f, 1.0f);
            glClear(GL_COLOR_BUFFER_BIT);
            glm::mat4 U = glm::ortho(0.0f, (float)GW_SCR_WIDTH, (float)GW_SCR_HEIGHT, 0.0f);

            // center
            float cx = GW_SCR_WIDTH * 0.5f;
            float cy = GW_SCR_HEIGHT * 0.5f;

            // Big title: HELP DUCKKY (top of center)
            std::string bigTitle = "HELP DUCKKY...";
            float titleScale = 1.8f;
            float estTitleH = 56.0f * titleScale;
            float titleX = cx - 260.0f; // visual tweak
            float titleY_top = cy - 100.0f - estTitleH * 0.5f;
            float titleY_bl = GW_SCR_HEIGHT - (titleY_top + estTitleH) + 6.0f;
            RenderTextProg(textProg, bigTitle, titleX, titleY_bl, titleScale, glm::vec3(0.98f, 0.98f, 1.0f));

            // Decorative thin bar under title
            float barW = GW_SCR_WIDTH * 0.48f;
            float barH = 8.0f;
            float barX = cx - barW * 0.5f;
            float barY = titleY_top + estTitleH + 12.0f; // top-left origin for rect
            gw_drawUIRect(barX, barY, barW, barH, U, glm::vec3(0.12f, 0.46f, 0.88f));

            // START button centered beneath
            float boxW = GW_SCR_WIDTH * 0.36f;
            float boxH = 84.0f;
            float boxX = cx - boxW * 0.5f;
            float boxY = barY + barH + 36.0f;

            // shadow and box
            gw_drawUIRect(boxX + 8.0f, boxY + 8.0f, boxW, boxH, U, glm::vec3(0.02f, 0.02f, 0.02f));
            gw_drawUIRect(boxX, boxY, boxW, boxH, U, glm::vec3(0.12f, 0.46f, 0.88f));
            gw_drawUIRect(boxX + 6.0f, boxY + 6.0f, boxW - 12.0f, 6.0f, U, glm::vec3(1.0f) * 0.10f);

            // START text (convert to bottom-left coords)
            std::string startText = "START";
            float startScale = 1.1f;
            float estStartH = 48.0f * startScale;
            float startTextX = boxX + boxW * 0.5f - 36.0f;
            float startTextY_top = boxY + (boxH * 0.5f) - (estStartH * 0.5f);
            float startTextY_bl = GW_SCR_HEIGHT - (startTextY_top + estStartH) + (estStartH * 0.05f);
            RenderTextProg(textProg, startText, startTextX + 1.5f, startTextY_bl + 1.5f, startScale, glm::vec3(0.03f));
            RenderTextProg(textProg, startText, startTextX, startTextY_bl, startScale, glm::vec3(1.0f));

            // input: click the box or press Enter/Space
            double mx, my; glfwGetCursorPos(win, &mx, &my);
            bool hovered = gw_pointInRect(mx, my, boxX, boxY, boxW, boxH);
            bool clicked = (glfwGetMouseButton(win, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS) && hovered;
            bool enter = (glfwGetKey(win, GLFW_KEY_ENTER) == GLFW_PRESS) || (glfwGetKey(win, GLFW_KEY_SPACE) == GLFW_PRESS);

            if (clicked || enter) {
                g_state = PLAYING;
                g_killedAnyGhost = false;
                gw_resetGame(player, ghosts, bullets, hasGun, fireCooldown);
                glEnable(GL_DEPTH_TEST);
                gw_updateTitle(win);
            }

            glfwSwapBuffers(win);
            continue;
        }
        if (g_state == WON) {

            // 2D UI setup
            glViewport(0, 0, GW_SCR_WIDTH, GW_SCR_HEIGHT);
            glDisable(GL_DEPTH_TEST);
            glClearColor(0.01f, 0.02f, 0.04f, 1.0f);
            glClear(GL_COLOR_BUFFER_BIT);

            glm::mat4 U = glm::ortho(0.0f, (float)GW_SCR_WIDTH, (float)GW_SCR_HEIGHT, 0.0f);

            // Card geometry (centered)
            float cardW = 680.0f;
            float cardH = 300.0f;
            float cardX = (GW_SCR_WIDTH - cardW) * 0.5f;
            float cardY = (GW_SCR_HEIGHT - cardH) * 0.5f;

            // Card background and subtle shadow
            gw_drawUIRect(cardX + 10.0f, cardY + 12.0f, cardW, cardH, U, glm::vec3(0.02f, 0.02f, 0.03f)); // shadow
            gw_drawUIRect(cardX, cardY, cardW, cardH, U, glm::vec3(0.06f, 0.07f, 0.10f)); // card

            // Convert Y for RenderTextProg (bottom-left origin)
            auto toTextY = [&](float y_top, float estPxH) {
                return GW_SCR_HEIGHT - (y_top + estPxH);
                };

            // Title (YOU WIN / YOU LOSE) - compute ribbon at top of card and center title in ribbon
            std::string winTitle = g_lastWasLose ? std::string("YOU LOSE!") : std::string("YOU WIN!");
            glm::vec3 titleColor = g_lastWasLose ? glm::vec3(0.95f, 0.35f, 0.35f) : glm::vec3(0.95f, 0.95f, 0.3f);
            float titleScale = 1.2f;
            float estTitleH = 48.0f * titleScale; // estimate

            // ribbon position: near top of card, enough height to fully contain title
            float ribbonPadH = 20.0f;
            float ribbonY = cardY + 12.0f;
            float ribbonH = estTitleH + ribbonPadH;
            gw_drawUIRect(cardX + 8.0f, ribbonY, cardW - 16.0f, ribbonH, U, glm::vec3(0.12f, 0.46f, 0.88f));

            // title top-left Y: place centered inside ribbon vertically
            float titleY_top = ribbonY + (ribbonH - estTitleH) * 0.5f;
            // tweak X so title looks more visually centered
            float titleX = cardX + (cardW * 0.5f) - 85.0f;
            float titleY_bl = toTextY(titleY_top, estTitleH);
            RenderTextProg(textProg, winTitle, titleX, titleY_bl, titleScale, titleColor);

            // Message lines below ribbon; smaller font size
            std::string msg = g_lastWasLose ? std::string("You were caught by an enemy!") : std::string("Collected enough coins!");
            std::string res = "Coins collected: " + std::to_string(g_winCoins) + " / " + std::to_string(g_requiredCoins);
            float msgScale = 0.72f; // reduced size
            float estMsgH = 30.0f * msgScale;
            float msgY_top = ribbonY + ribbonH + 26.0f;   // a bit more spacing
            float msgY_bl = toTextY(msgY_top, estMsgH);
            float msgX = cardX + 60.0f;                   // nicer left margin
            RenderTextProg(textProg, msg, msgX, msgY_bl, msgScale, glm::vec3(0.95f, 0.95f, 0.95f));
            RenderTextProg(textProg, res, msgX, msgY_bl - (estMsgH + 24.0f), msgScale, glm::vec3(0.95f, 0.95f, 0.95f));

            // Decorative small squares (bottom-right of card)
            gw_drawUIRect(cardX + cardW - 44.0f, cardY + cardH - 44.0f, 10.0f, 10.0f, U, glm::vec3(0.18f, 0.6f, 0.95f));
            gw_drawUIRect(cardX + cardW - 28.0f, cardY + cardH - 60.0f, 8.0f, 8.0f, U, glm::vec3(0.9f, 0.6f, 0.2f));

            // RETRY / RESTART button centered near bottom
            float btnW = 220.0f, btnH = 68.0f;
            float btnX = cardX + (cardW - btnW) * 0.5f;
            float btnY = cardY + cardH - btnH - 28.0f;
            gw_drawUIRect(btnX + 2.0f, btnY + 6.0f, btnW, btnH, U, glm::vec3(0.02f, 0.02f, 0.02f));
            gw_drawUIRect(btnX, btnY, btnW, btnH, U, glm::vec3(0.16f, 0.48f, 0.88f));

            // label
            std::string btnTxt = g_lastWasLose ? std::string("RETRY") : std::string("RESTART");
            float btnScale = 0.95f;
            float estBtnH = 40.0f * btnScale;
            float btnTxtX = btnX + (btnW * 0.5f) - 46.0f;
            float btnTxtY_top = btnY + (btnH * 0.5f) - (estBtnH * 0.5f);
            float btnTxtY_bl = toTextY(btnTxtY_top, estBtnH);
            RenderTextProg(textProg, btnTxt, btnTxtX, btnTxtY_bl, btnScale, glm::vec3(1.0f));

            // Input: clicking or pressing Enter/Space will restart and reset coins
            double mx, my; glfwGetCursorPos(win, &mx, &my);
            bool hovered = gw_pointInRect(mx, my, btnX, btnY, btnW, btnH);
            bool click = (glfwGetMouseButton(win, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS) && hovered;
            bool enterKey = (glfwGetKey(win, GLFW_KEY_ENTER) == GLFW_PRESS) || (glfwGetKey(win, GLFW_KEY_SPACE) == GLFW_PRESS);
            if (click || enterKey) {
                g_state = PLAYING;
                g_winCoins = 0;
                g_lastWasLose = false;
                g_killedAnyGhost = false;
                g_coinCollected = 0;
                gw_resetGame(player, ghosts, bullets, hasGun, fireCooldown);
                g_titleDirty = true;
                gw_updateTitle(win);
                glEnable(GL_DEPTH_TEST);
            }

            glfwSwapBuffers(win);
            continue;
        }





        // ======= GAMEPLAY =======

        // RMB orbit
        static const float DEADZONE = 2.0f;
        int rmb = glfwGetMouseButton(win, GLFW_MOUSE_BUTTON_RIGHT);
        if (rmb == GLFW_PRESS && !rotating) {
            rotating = true; rmbPrimed = true; glfwGetCursorPos(win, &lastMX, &lastMY);
#ifdef GLFW_RAW_MOUSE_MOTION
            if (glfwRawMouseMotionSupported()) glfwSetInputMode(win, GLFW_RAW_MOUSE_MOTION, GLFW_TRUE);
#endif
            glfwSetInputMode(win, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
        }
        if (rmb == GLFW_RELEASE && rotating) {
            rotating = false;
#ifdef GLFW_RAW_MOUSE_MOTION
            if (glfwRawMouseMotionSupported()) glfwSetInputMode(win, GLFW_RAW_MOUSE_MOTION, GLFW_FALSE);
#endif
            glfwSetInputMode(win, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
        }
        if (rotating) {
            double mx, my; glfwGetCursorPos(win, &mx, &my);
            if (rmbPrimed) { lastMX = mx; lastMY = my; rmbPrimed = false; }
            else {
                double dx = mx - lastMX, dy = my - lastMY; lastMX = mx; lastMY = my;
                if (std::abs(dx) > DEADZONE || std::abs(dy) > DEADZONE) {
                    camYaw += float(dx) * SENS_X;
                    camPitch -= float(dy) * SENS_Y;
                    camPitch = std::max(CAM_PITCH_MIN, std::min(CAM_PITCH_MAX, camPitch));
                    if (camYaw > 180.f) camYaw -= 360.f;
                    if (camYaw < -180.f) camYaw += 360.f;
                }
            }
        }

        // Player movement (tile-by-tile) + pre-turn
        {
            glm::ivec2 in = gw_readInput(win);
            if (in != glm::ivec2(0)) player.ctrl.queued = in;

            auto apply_dir = [&](const glm::ivec2& d) {
                player.ctrl.dir = d;
                if (d == glm::ivec2(1, 0))      player.yaw = 0.f;
                else if (d == glm::ivec2(-1, 0)) player.yaw = 180.f;
                else if (d == glm::ivec2(0, 1))  player.yaw = 90.f;
                else if (d == glm::ivec2(0, -1)) player.yaw = -90.f;
                };

            if (!player.ctrl.moving && player.ctrl.queued != glm::ivec2(0)) {
                glm::ivec2 t = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
                glm::ivec2 nt = t + player.ctrl.queued;
                if (!gw_wallAt(nt.x, nt.y)) {
                    apply_dir(player.ctrl.queued);
                    player.ctrl.target = gw_centerOf(nt);
                    player.ctrl.moving = true;
                }
            }

            if (player.ctrl.moving && player.ctrl.queued != glm::ivec2(0) && player.ctrl.queued != player.ctrl.dir) {
                bool orthogonal = (player.ctrl.queued.x == 0 && player.ctrl.dir.x != 0) ||
                    (player.ctrl.queued.y == 0 && player.ctrl.dir.y != 0);
                if (orthogonal) {
                    glm::ivec2 t = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
                    glm::vec2  center = gw_centerOf(t);

                    glm::ivec2 turnTo = t + player.ctrl.queued;
                    if (!gw_wallAt(turnTo.x, turnTo.y)) {
                        const float TURN_SNAP = 0.20f;
                        glm::vec2  toC = center - player.pos;
                        float      distC = glm::length(toC);

                        float step = GW_STEP_SPEED_PLAYER * dt;
                        bool willCrossCenter = (distC <= step + 1e-4f);

                        if (distC <= TURN_SNAP || willCrossCenter) {
                            player.pos = center;
                            apply_dir(player.ctrl.queued);
                            player.ctrl.target = gw_centerOf(turnTo);
                            player.ctrl.moving = true;
                        }
                    }
                }
            }

            if (player.ctrl.moving) {
                glm::vec2 to = player.ctrl.target - player.pos;
                float dist = glm::length(to);
                if (dist < 1e-4f) {
                    player.pos = player.ctrl.target;
                    player.ctrl.moving = false;
                }
                else {
                    glm::vec2 v = (to / std::max(dist, 1e-6f)) * GW_STEP_SPEED_PLAYER;
                    float step = GW_STEP_SPEED_PLAYER * dt;
                    if (step >= dist) { player.pos = player.ctrl.target; player.ctrl.moving = false; }
                    else player.pos += v * dt;
                }
            }

            // Pickups from walking
            glm::ivec2 pt = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
            if (pt.x >= 0 && pt.x < GW_GRID_W && pt.y >= 0 && pt.y < GW_GRID_H) {
                gw_processTilePickupAt(pt, player, ghosts, bullets, hasGun, fireCooldown, win);
            }
        }

        // Shooting input (space short press only now)
        {
            static bool spaceWasDown = false;
            static double spacePressedAt = 0.0;
            bool spaceNow = (glfwGetKey(win, GLFW_KEY_SPACE) == GLFW_PRESS);

            if (spaceNow && !spaceWasDown) {
                // started pressing
                spacePressedAt = now;
            }

            if (!spaceNow && spaceWasDown) {
                // released -> treat as shoot only (jump disabled by threshold)
                double held = now - spacePressedAt;
                // since JUMP_HOLD_THRESHOLD is huge, this will always be < threshold, so shoot
                if (hasGun && fireCooldown <= 0.0f) {
                    glm::vec2 shootDir(0, -1);
                    if (player.ctrl.dir != glm::ivec2(0)) shootDir = glm::vec2((float)player.ctrl.dir.x, (float)player.ctrl.dir.y);
                    else {
                        if (fabs(player.yaw - 0.f) < 1e-3f)        shootDir = { 1, 0 };
                        else if (fabs(player.yaw - 180.f) < 1e-1f) shootDir = { -1, 0 };
                        else if (fabs(player.yaw - 90.f) < 1e-1f)  shootDir = { 0, 1 };
                        else if (fabs(player.yaw + 90.f) < 1e-1f)  shootDir = { 0,-1 };
                    }
                    if (glm::length(shootDir) > 0.0f) {
                        bullets.push_back(GWBullet{ player.pos, glm::normalize(shootDir), 1.5f, true });
                        fireCooldown = GW_FIRE_COOLDOWN;
                    }
                }
            }

            spaceWasDown = spaceNow;
        }

        // Ghosts
        glm::ivec2 playerTile = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
        for (auto& g : ghosts) {
            glm::ivec2 gt = { (int)std::floor(g.pos.x), (int)std::floor(g.pos.y) };
            if (!g.ctrl.moving) {
                glm::ivec2 ndir = gw_chooseDirChase(gt, g.ctrl.dir, playerTile);
                if (ndir != glm::ivec2(0)) {
                    g.ctrl.dir = ndir;
                    g.ctrl.target = gw_centerOf(gt + ndir);
                    g.ctrl.moving = true;
                    if (g.ctrl.dir == glm::ivec2(1, 0))  g.yaw = 0.f;
                    else if (g.ctrl.dir == glm::ivec2(-1, 0)) g.yaw = 180.f;
                    else if (g.ctrl.dir == glm::ivec2(0, 1))  g.yaw = 90.f;
                    else if (g.ctrl.dir == glm::ivec2(0, -1)) g.yaw = -90.f;
                }
            }
            if (g.ctrl.moving) {
                glm::vec2 to = g.ctrl.target - g.pos;
                float dist = glm::length(to);
                if (dist < 1e-4f) { g.pos = g.ctrl.target; g.ctrl.moving = false; }
                else {
                    glm::vec2 v = (to / dist) * GW_STEP_SPEED_ENEMY;
                    float step = GW_STEP_SPEED_ENEMY * dt;
                    if (step >= dist) { g.pos = g.ctrl.target; g.ctrl.moving = false; }
                    else g.pos += v * dt;
                }
            }
        }

        // Bullets update
        for (auto& b : bullets) {
            if (!b.alive) continue;
            b.pos += b.dir * GW_BULLET_SPEED * dt;
            b.life -= dt;
            if (b.life <= 0.0f) b.alive = false;
            glm::ivec2 bt = { (int)std::floor(b.pos.x), (int)std::floor(b.pos.y) };
            if (bt.x < 0 || bt.x >= GW_GRID_W || bt.y < 0 || bt.y >= GW_GRID_H || gw_wallAt(bt.x, bt.y)) b.alive = false;
        }
        bullets.erase(std::remove_if(bullets.begin(), bullets.end(), [](const GWBullet& x) {return !x.alive; }), bullets.end());

        // Bullet vs Ghost
        for (auto itg = ghosts.begin(); itg != ghosts.end();) {
            bool killed = false;
            for (auto& b : bullets) {
                if (!b.alive) continue;
                if (glm::length(b.pos - itg->pos) < 0.7f) { b.alive = false; killed = true; break; }
            }
            if (killed) {
                std::cout << "Ghost shot!\n";
                g_killedAnyGhost = true;
                itg = ghosts.erase(itg);
                // after erasing, check win condition: require coins >= required AND player killed at least one ghost AND no ghosts left
                if (g_coinCollected >= g_requiredCoins && g_killedAnyGhost && ghosts.empty()) {
                    // Win — show WIN screen
                    g_lastWasLose = false;
                    g_state = WON;
                    g_winCoins = g_coinCollected;
                    // clear bullets and freeze
                    bullets.clear();
                    // title update
                    g_titleDirty = true;
                    break; // exit loop because ghosts vector changed
                }
            }
            else ++itg;
        }

        // Player vs Ghost — show RESTART screen (เฉพาะตอนอยู่ในแมพหลัก)
        if (!g_inBonus) {
            bool collided = false;
            for (auto& g : ghosts) {
                if (glm::length(g.pos - player.pos) < 0.55f) { collided = true; break; }
            }
            if (collided) {
                std::cout << "Caught! Player lost. Switching to LOSE screen.\n";
                // mark as lose and switch to WIN/RESTART UI (we reuse WON state but show Lose text)
                g_lastWasLose = true;
                g_state = WON;
                g_winCoins = g_coinCollected;
                bullets.clear();
                g_titleDirty = true;
            }
        }

        // Gun respawn system (ทำเฉพาะในแมพหลัก)
        if (!g_inBonus) {
            bool hasK = false;
            for (int y = 0; y < GW_GRID_H && !hasK; ++y)
                for (int x = 0; x < GW_GRID_W; ++x)
                    if (GW_MAP[y][x] == 'K') { hasK = true; break; }

            if (!hasK) {
                gunRespawnTimer -= dt;
                if (gunRespawnTimer <= 0.0f) {
                    std::vector<glm::ivec2> avoid;
                    glm::ivec2 ppos = { (int)std::floor(player.pos.x), (int)std::floor(player.pos.y) };
                    if (ppos.x >= 0 && ppos.x < GW_GRID_W && ppos.y >= 0 && ppos.y < GW_GRID_H) avoid.push_back(ppos);
                    for (auto& g : ghosts) avoid.push_back({ (int)std::floor(g.pos.x), (int)std::floor(g.pos.y) });

                    if (gw_placeRandomGun(avoid, 512, 1)) {
                        gunRespawnTimer = GUN_RESPAWN_INTERVAL;
                    }
                    else {
                        gunRespawnTimer = 1.0f;
                    }
                }
            }
            else {
                gunRespawnTimer = GUN_RESPAWN_INTERVAL;
            }
        }

        // ===== Bonus timer: ครบ 20 วิแล้วกลับแมพเดิม =====
        if (g_inBonus) {
            g_bonusTimer -= dt;
            if (g_bonusTimer <= 0.0f) {
                std::cout << "Bonus time over, returning to main map.\n";

                // เก็บจำนวนเหรียญที่เก็บในแมพโบนัสไว้ก่อน
                int bonusCollected = g_coinCollected;
                int bonusTotal = g_coinTotal;

                g_inBonus = false;

                // คืน state แมพหลัก (รวม coin ของแมพหลัก)
                gw_restoreState(g_savedMainState, player, ghosts, bullets, hasGun, fireCooldown);

                // เพิ่มเหรียญที่เก็บจากโบนัสเข้าไปในตัวนับรวม
                g_coinCollected += bonusCollected;
                // NEW: check win after bonus merge
                if (g_coinCollected >= g_requiredCoins) {
                    g_lastWasLose = false;
                    g_state = WON;
                    g_winCoins = g_coinCollected;
                    bullets.clear();
                    g_titleDirty = true;
                }
                g_coinTotal += bonusTotal;
                g_titleDirty = true;
            }
        }

        if (fireCooldown > 0.0f) fireCooldown -= dt;

        // Camera
        glm::vec3 target = { player.pos.x, 0.15f + player.elev + 0.0f, player.pos.y };
        float yawRad = glm::radians(camYaw), pitchRad = glm::radians(camPitch);
        glm::vec3 dir;
        dir.x = std::cos(pitchRad) * std::sin(yawRad);
        dir.y = std::sin(pitchRad);
        dir.z = std::cos(pitchRad) * std::cos(yawRad);
        glm::vec3 camPos = target - dir * camDist;

        glm::mat4 V = glm::lookAt(camPos, target, { 0,1,0 });
        glm::mat4 P = glm::perspective(glm::radians(55.f), (float)GW_SCR_WIDTH / (float)GW_SCR_HEIGHT, 0.1f, 200.f);

        // ===== Render =====
        glViewport(0, 0, GW_SCR_WIDTH, GW_SCR_HEIGHT);
        glClearColor(0.02f, 0.03f, 0.06f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // ===== Floor + Walls + Items + Coins =====
        for (int y = 0; y < GW_GRID_H; ++y) {
            for (int x = 0; x < GW_GRID_W; ++x) {
                char c = GW_MAP[y][x];

                // Floor
                {
                    bool alt = ((x + y) & 1);
                    glm::vec3 floorColor = alt ? glm::vec3(0.08f, 0.09f, 0.13f)
                        : glm::vec3(0.10f, 0.12f, 0.16f);
                    gw_drawCube(V, P, { x + 0.5f, -0.01f, y + 0.5f }, { 1, 0.02f, 1 }, floorColor);
                }

                if (c == '#') {
                    bool alt = ((x + y) & 1);
                    glm::vec3 wallColor = alt ? glm::vec3(0.12f, 0.35f, 0.85f)
                        : glm::vec3(0.10f, 0.30f, 0.76f);
                    gw_drawCube(V, P, { x + 0.5f, 0.5f, y + 0.5f }, { 1, 1, 1 }, wallColor);
                }
                else if (c == 'K') {
                    glm::vec3 gp = { x + 0.5f, 0.15f, y + 0.5f };
                    gw_drawModel(modelShader, gunModel, V, P, gp, glm::vec3(0.0012f), 0.f, -90.f, 0.f);
                }
                else if (c == 'M') {
                    gw_drawSphere(V, P, { x + 0.5f, 0.18f, y + 0.5f }, 0.12f, { 0.2f, 0.9f, 0.35f });
                }
                else if (c == 'C') {
                    gw_drawSphere(V, P, { x + 0.5f, 0.14f, y + 0.5f }, 0.10f, { 1.0f, 0.85f, 0.25f });
                }
            }
        }

        // Player
        float faceYaw = (player.ctrl.dir.y != 0) ? (player.yaw - 90.f) : (player.yaw + 90.f);
        float playerBaseY = 0.15f + player.elev; // elev stays zero since jump is disabled
        gw_drawModel(modelShader, playerModel, V, P,
            { player.pos.x, playerBaseY, player.pos.y },
            glm::vec3(1.0f),
            faceYaw, 0.f, 0.f);

        // Ghosts
        const glm::vec3 GHOST_SCL = glm::vec3(0.35f);
        const float     GHOST_Y = 0.25f;
        const float     GHOST_PIT = 0.0f;

        for (auto& g : ghosts) {
            gw_drawModel(modelShader, ghostModel, V, P,
                { g.pos.x, GHOST_Y, g.pos.y },
                GHOST_SCL,
                g.yaw, GHOST_PIT, 0.f);

            gw_drawSphere(V, P, { g.pos.x, GHOST_Y + 0.10f, g.pos.y }, 0.10f, { 0.9f, 0.85f, 0.2f });
        }

        // Bullets
        for (auto& b : bullets)
            gw_drawSphere(V, P, { b.pos.x, 0.10f, b.pos.y }, 0.08f, { 1.0f, 0.95f, 0.2f });

        // Skybox
        glDepthFunc(GL_LEQUAL);
        skyboxShaderPtr->use();
        glm::mat4 viewNoTrans = glm::mat4(glm::mat3(V));
        skyboxShaderPtr->setMat4("view", viewNoTrans);
        skyboxShaderPtr->setMat4("projection", P);

        glBindVertexArray(skyboxVAO);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_CUBE_MAP, cubemapTexture);
        glDrawArrays(GL_TRIANGLES, 0, 36);
        glBindVertexArray(0);
        glDepthFunc(GL_LESS);

        // HUD (text + icons)
        glDisable(GL_DEPTH_TEST);
        glm::mat4 Ortho = glm::ortho(0.0f, (float)GW_SCR_WIDTH, 0.0f, (float)GW_SCR_HEIGHT);

        // coin box
        float padding = 12.0f;
        float coinSize = 18.0f;
        float ux = padding, uy = GW_SCR_HEIGHT - padding - coinSize;
        gw_drawUIRect(ux - 4, uy - 4, coinSize + 8, coinSize + 8, Ortho, glm::vec3(0.02f, 0.03f, 0.06f));
        gw_drawUIRect(ux, uy, coinSize, coinSize, Ortho, glm::vec3(1.0f, 0.85f, 0.25f));
        std::string nums = std::to_string(g_coinCollected) + "/" + std::to_string(g_coinTotal);
        float tx = ux + coinSize + 8.0f;
        float ty = uy + 2.0f; // small offset

        // Render coin count (using RenderTextProg; note RenderTextProg uses bottom-left origin)
        RenderTextProg(textProg, nums, tx, ty, 0.6f, glm::vec3(1.0f, 1.0f, 1.0f));

        // Gun status
        std::string gunTxt = hasGun ? "Gun: YES" : "Gun: NO";
        RenderTextProg(textProg, gunTxt, 12.0f, 30.0f, 0.5f, glm::vec3(1.0f, 1.0f, 1.0f));

        glEnable(GL_DEPTH_TEST);

        if (g_titleDirty) {
            gw_updateTitle(win);
        }

        glfwSwapBuffers(win);
    }

    glfwTerminate();
    return 0;
}
