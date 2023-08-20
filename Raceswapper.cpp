Begin Cloning.cpp:
#include "Cloning.h"
#include "swap/NPCAppearance.h"
bool Cloning(RE::StaticFunctionTag*, RE::Actor* actorToCopy, RE::Actor
* actorToSet)
{
	logger::info("Size of RE::TESNPC::Layer: {} bytes", sizeof(RE::TESNPC
::Layer));
	if (!actorToCopy || !actorToSet)
		return false;
	auto npcToCopy = actorToCopy->GetActorBase();
	auto npcToSet = actorToSet->GetActorBase();
	if (!npcToCopy || !npcToSet)
		return false;
	auto appearanceToCopy = NPCAppearance::GetOrCreateNPCAppearance(npcTo
Copy);
	auto appearanceToSet = NPCAppearance::GetOrCreateNPCAppearance(npcToS
et);
	if (!appearanceToCopy || !appearanceToSet)
		return false;
	// Copiez l’apparence
	appearanceToSet->ApplyNewAppearance(false); // false indique de ne p
as mettre ˆ  jour les acteurs chargˆ's
	return true;
}
bool Papyrus_Database(RE::BSScript::IVirtualMachine* a_vm)
{
	a_vm->RegisterFunction("Cloning", "SBP", Cloning);
	return true;
}
//========================================End Cloning.cpp
Begin Cloning.h:
#pragma once
#include "Papyrus.h"
bool Cloning(RE::StaticFunctionTag*, RE::Actor* actorToCopy, RE::Actor
* actorToSet);
//========================================End Cloning.h
Begin Configuration.cpp:
#pragma once
#include "Configuration.h"
#include "Utils.h"
void ConfigurationDatabase::Initialize()
{
	logger::info("Reading config APIs...");
	constexpr auto path = L"Data/SKSE/Plugins/RaceSwap";
	// Vˆ'rifiez si le rˆ'pertoire existe
	std::filesystem::path dirPath = path;
	if (!std::filesystem::exists(dirPath)) {
		// Crˆ'ez le rˆ'pertoire s’il n’existe pas
		std::filesystem::create_directories(dirPath);
	}
	try {
		for (const auto& entry : std::filesystem::directory_iterator(path)) 
{
			logger::info("Parsing file {}", entry.path().string().c_str());
			std::fstream config;
			config.open(entry.path(), std::ios::in);
			if (!config.is_open()) {
				logger::error("Couldn’t open file {}", entry.path().string().c_str
());
				continue;
			}
			std::string line;
			while (std::getline(config, line)) {
				auto configEntry = ConfigurationEntry::ConstructNewEntry(line);
				if (configEntry) {
					entries.push_back(configEntry);
				}
			}
		}
	} catch (const std::exception& e) {
		logger::error("Error while reading config files: {}", e.what());
	}
	logger::info("Config APIs fully parsed!");
}
ConfigurationEntry* PickRandomWeightedEntry(std::vector<std::pair<std:
:uint32_t, ConfigurationEntry*>> a_entries, RE::TESNPC* a_npc)
{
	if (a_entries.empty()) {
		return nullptr;	
	}
	if (a_entries.size() == 1) {
		return a_entries[0].second;
	}
	std::vector<std::uint32_t> weights(a_entries.size(), 0);
	weights[0] = a_entries[0].first;
	for (std::uint32_t i = 1; i < a_entries.size(); i++) {
		weights[i] = a_entries[i].first + weights[i-1]; 
	}
	auto seed = utils::HashForm(a_npc);
	srand((int) seed);
	auto index = std::upper_bound(weights.begin(), weights.end(), rand() 
% weights.back()) - weights.begin();
	return a_entries[index].second;
}
AppearanceConfiguration* ConfigurationDatabase::GetConfigurationForNPC
(RE::TESNPC* a_npc) {
	std::vector<std::pair<std::uint32_t, ConfigurationEntry*>> npcSwapEnt
ries;
	std::vector<std::pair<std::uint32_t, ConfigurationEntry*>> raceSwapEn
tries;
	for (auto entry : entries) {
		if (entry->MatchesNPC(a_npc)) {
			if (entry->entryData.otherNPC) {
				npcSwapEntries.push_back({ entry->entryData.weight, entry });
			} else if (entry->entryData.otherRace) {
				raceSwapEntries.push_back({ entry->entryData.weight, entry });
			}
		}
	}
	if (!npcSwapEntries.empty()) {
		auto config = new AppearanceConfiguration{ 0 };
		config->otherNPC = PickRandomWeightedEntry(npcSwapEntries, a_npc)->e
ntryData.otherNPC;
		return config;
	}
	if (!raceSwapEntries.empty()) {
		auto config = new AppearanceConfiguration{ 0 };
		config->otherRace = PickRandomWeightedEntry(raceSwapEntries, a_npc)-
>entryData.otherRace;
		return config;
	}
	return nullptr;
}
//========================================End Configuration.cpp
Begin Configuration.h:
#pragma once
#include <utility>
#include "ConfigurationEntry.h"
struct AppearanceConfiguration
{
	RE::TESNPC* otherNPC;
	RE::TESRace* otherRace;
};
/* Example Config file
# Everything after a hashtag is a comment
# Matches accept the following: Race, NPC, Faction, Probability (defau
lt: 100%, max 100%)
# Swaps accept the following: Race, NPC, Weight (default 10, max 100)
# Weight is used when determining which swap to apply
# Ex: Nazeem matches a nord race swap AND a khajiit race swap. The hig
her weight entry is more
# likely to get the swap over the other, but not guaranteed
# Swap all nords to Khajiit with a 50% chance
match=0x13746~Skyrim.esm|50% swap=0x13745~Skyrim.esm
# Swap all nords to Khajiit with a 100% chance using editor IDs
match=NordRace|100% swap=KhajiitRace # Anything after the "#" are igno
red (use as comment)
# Swap all nords to Argonians, using a high weight
match=NordRace|100% swap=ArgonianRace|100 # This entry very likely to
 win against the others with default weight of 10 
*/
class ConfigurationDatabase
{
public:
	inline static ConfigurationDatabase*& GetSingleton()
	{
		static ConfigurationDatabase* _this_database = nullptr;
		if (!_this_database)
			_this_database = new ConfigurationDatabase();
		return _this_database;
	}
	/*
	@brief Safely de-allocate the memory space used by DataBase.
	*/
	static void Dealloc()
	{
		delete GetSingleton();
		GetSingleton() = nullptr;
	}
	// Load entries from the various entry files
	void Initialize();
	AppearanceConfiguration* GetConfigurationForNPC(RE::TESNPC* a_npc);
private:
	std::vector<ConfigurationEntry*> entries;
};
//========================================End Configuration.h
Begin ConfigurationEntry.cpp:
#pragma once
#include "ConfigurationEntry.h"
#include "Utils.h"
#include <algorithm>
bool IsValidEntry(std::string line) {
	if (line.find("match=") == std::string::npos) {
		logger::error("line: \"{}\" is missing a ’match=’ line and cannot be
 parsed!", line);
		return false;
	}
	if (line.find("match=") != 0) {
		logger::error("line: \"{}\" is invalid, ’match=’ must be first in th
e line!", line);
	}
	if (line.find("swap=") == std::string::npos) {
		logger::error("line: \"{}\" is missing a ’swap=’ line and cannot be 
parsed!", line);
		return false;
	}
	return true;
}
RE::TESForm* GetFormFromString(std::string line) {
	if (line.find(’~’) == std::string::npos) {
		logger::error("missing plugin: {}", line);
	}
	auto plugin = line.substr(line.find(’~’) + 1);
	auto formID = std::stoul(line.substr(0, line.find(’~’)), nullptr, 16)
;
	auto form = RE::TESDataHandler::GetSingleton()->LookupForm(formID, pl
ugin);
	if (form == nullptr) {
		logger::error("invalid form ID: {}", line);
	}
	return form;
}
bool ConstructMatchData(std::string line, ConfigurationEntry::EntryDat
a* a_data)
{
	std::string match = "match=";
	line.erase(0, match.size());
	auto data = utils::split_string(line, ’|’);
	for (auto& entry : data) {
		if (entry.find(’%’) != std::string::npos) {
			auto percent = std::stoul(entry.substr(0, entry.find(’%’)), nullptr
, 10);
			percent = std::max<unsigned long>(0, std::min<unsigned long>(100, p
ercent));
			a_data->probability = percent;
		} else {
			auto form = RE::TESForm::LookupByEditorID(entry);
			form = form ? form : GetFormFromString(entry);
			if (!form) {
				logger::error("{} is not a valid form ID or editor ID!", entry);
				return false;
			}
			if (form->Is(RE::FormType::NPC)) {
				a_data->npcMatch = form->As<RE::TESNPC>();
			} else if (form->Is(RE::FormType::Race)) {
				a_data->raceMatch = form->As<RE::TESRace>();
			} else if (form->Is(RE::FormType::Faction)) {
				a_data->factionMatch = form->As<RE::TESFaction>();
			} else {
				logger::error(
					"{} {:x} is not a valid NPC, Race or Faction!",
					utils::GetFormEditorID(form).c_str(),
					form->formID
				);
				return false;
			}
		}
	}
	return true;
}
bool ConstructSwapData(std::string line, ConfigurationEntry::EntryData
* a_data)
{
	std::string swap = "swap=";
	line.erase(0, swap.size());
	auto data = utils::split_string(line, ’|’);
	for (auto& entry : data) {
		if (entry.find(’%’) != std::string::npos) {
			auto percent = std::stoul(entry.substr(0, entry.find(’%’)), nullptr
, 10);
			percent = std::max<unsigned long>(0, std::min<unsigned long>(100, p
ercent));
			a_data->weight = percent;
		} else {
			auto form = RE::TESForm::LookupByEditorID(entry);
			form = form ? form : GetFormFromString(entry);
			if (!form) {
				logger::error("{} is not a valid form ID or editor ID!", entry);
				return false;
			}
			if (form->Is(RE::FormType::NPC)) {
				a_data->otherNPC = form->As<RE::TESNPC>();
			} else if (form->Is(RE::FormType::Race)) {
				a_data->otherRace = form->As<RE::TESRace>();
			} else {
				logger::error(
					"{} {:x} is not a valid NPC or Race!",
					utils::GetFormEditorID(form).c_str(),
					form->formID
				);
				return false;
			}
		}
	}
	return true;
}
ConfigurationEntry* ConfigurationEntry::ConstructNewEntry(std::string 
line)
{
	auto parsingLine = std::string(line);
	ConfigurationEntry::EntryData entryData{ 0 };
	/////////// Default Values /////////////
	entryData.weight = 10;
	entryData.probability = 100;
	////////////////////////////////////////
	// Strip the comments and the whitespace
	if (parsingLine.find(’#’) != std::string::npos) {
		parsingLine.erase(parsingLine.find(’#’));
	}
	// Remove the whitespace
	parsingLine.erase(remove(parsingLine.begin(), parsingLine.end(), ’ ’)
, parsingLine.end());
	if (parsingLine.empty() || !IsValidEntry(parsingLine)) {
		// Line is invalid, or was just a comment. Either way, don’t parse i
t
		return nullptr;
	}
	logger::info("Parsing: {}", line);
	// TODO: Make this case insensitive
	auto swapIndex = parsingLine.find("swap=");
	auto matchLine = parsingLine.substr(0, swapIndex);
	auto swapLine = parsingLine.substr(swapIndex);
	bool success = false;
	try {
		success = ConstructMatchData(matchLine, &entryData) && ConstructSwap
Data(swapLine, &entryData); 
	} catch (...) {
		logger::error("line: \"{}\" is invalid", line);
	}
	if (success) {
		auto newEntry = new ConfigurationEntry();
		newEntry->entryData = entryData;
		return newEntry;
	}
	logger::error("line: \"{}\" is invalid", line);
	return nullptr;	
}
bool ConfigurationEntry::MatchesNPC(RE::TESNPC* a_npc) {
	bool isMatch = false;
	isMatch = isMatch || (entryData.npcMatch != nullptr && entryData.npcM
atch->formID == a_npc->formID);
	isMatch = isMatch || (entryData.raceMatch != nullptr && entryData.rac
eMatch->formID == a_npc->race->formID);
	isMatch = isMatch || (entryData.factionMatch != nullptr && a_npc->IsI
nFaction(entryData.factionMatch));
	// Prevents child NPCs matching for adult swaps and vice-versa
	isMatch = isMatch && (!entryData.otherRace || a_npc->race->IsChildRac
e() == entryData.otherRace->IsChildRace());
	isMatch = isMatch && (!entryData.otherNPC || a_npc->race->IsChildRace
() == entryData.otherNPC->race->IsChildRace());
	if (isMatch) {
		// TODO: Hash should include the entry itself to prevent all entries
 with the same weight
		// matching the same exact NPCs
		srand((int) utils::HashForm(a_npc));
		isMatch = ((std::uint32_t) rand() % 100) < entryData.probability;
	}
	return isMatch;
}
//========================================End ConfigurationEntry.cpp
Begin ConfigurationEntry.h:
#pragma once
#include <utility>
class ConfigurationEntry
{
public:
	struct EntryData
	{
		// Matching Data
		RE::TESNPC* npcMatch = nullptr;
		RE::TESRace* raceMatch = nullptr;
		RE::TESFaction* factionMatch = nullptr;
		std::uint32_t probability = 100; // 0-100
		// Appearance Data
		RE::TESNPC* otherNPC = nullptr;
		RE::TESRace* otherRace = nullptr;
		std::uint32_t weight = 10; // 0-100
	};
	EntryData entryData;
	bool MatchesNPC(RE::TESNPC* a_npc);
	static ConfigurationEntry* ConstructNewEntry(std::string line);
};
//========================================End ConfigurationEntry.h
Begin Database.h:
#pragma once
//========================================End Database.h
Begin Hooks.cpp:
#pragma once
#include "Hooks.h"
#include "swap/NPCAppearance.h"
#include "Utils.h"
struct GetTESModelHook
{
	static RE::TESModel* GetTESModel(RE::TESNPC* a_npc)
	{
		NPCAppearance* appearance = NPCAppearance::GetNPCAppearance(a_npc);
		if (appearance != nullptr && appearance->isNPCSwapped) {
			return appearance->alteredNPCData.skeletonModel;
		}
		return OriginalModel(a_npc);
	}
	static RE::TESModel* OriginalModel(RE::TESNPC* a_npc) {
		// Original logic
		if (!a_npc->race->skeletonModels[a_npc->GetSex()].model.empty()) {
			return &a_npc->race->skeletonModels[a_npc->GetSex()];
		} else {
			return a_npc->race->skeletonModels;
		}
	}
	// Install our hook at the specified address
	static inline void Install()
	{
		REL::Relocation<std::uintptr_t> target{ RELOCATION_ID(19322, 19749),
 REL::VariantOffset(0x5F, 0x6B, 0x5F) };
		// Fill the gender and skeleton calls with NOP, as we will handle bo
th gender and skeleton access ourselves
		if (REL::Module::IsAE()) {
			REL::safe_fill(target.address(), REL::NOP, 0x32);
		} else {
			REL::safe_fill(target.address(), REL::NOP, 0xF);
		}
		auto& trampoline = SKSE::GetTrampoline();
		SKSE::AllocTrampoline(14);
		trampoline.write_call<5>(target.address(), reinterpret_cast<uintptr_
t>(GetTESModel));
		if (REL::Module::IsAE()) {
			// AE inlines and uses rbx for skeleton model
			std::byte fixReturnValue[] = { (std::byte)0x48, (std::byte)0x89, (s
td::byte)0xC3 }; // mov rbx, rax
			REL::safe_write(target.address() + 0x5, fixReturnValue, 3);
		}
		logger::info("GetTESModelHook hooked at address {:x}", target.addres
s());
		logger::info("GetTESModelHook hooked at offset {:x}", target.offset(
));
	}
};
struct GetBodyPartDataHook
{
	static RE::BGSBodyPartData* thunk(RE::TESRace* a_actor)
	{
		// a_npc WAS the race, but we kept it as Actor for our purposes >:)
		auto actor = reinterpret_cast<RE::Actor*>(a_actor);
		auto appearance = NPCAppearance::GetNPCAppearance(actor->GetActorBas
e());
		if (appearance && appearance->isNPCSwapped) {
			return appearance->alteredNPCData.bodyPartData;
		}
		if (!actor->GetActorRuntimeData().race) {
			return nullptr;
		}
		return func(actor->GetActorRuntimeData().race);
	}
	static inline REL::Relocation<decltype(thunk)> func;
	// Install our hook at the specified address
	static inline void Install()
	{
		REL::Relocation<std::uintptr_t> load3DTarget{ RELOCATION_ID(36198, 3
7177), REL::VariantOffset(0x5A, 0x57, 0x5A) };
		// Remove call to replace RCX (actor) with actor’s race. This lets o
ur hook have access to the actor data
		if (REL::Module::IsAE()) {
			REL::safe_fill(load3DTarget.address() - 0x14, REL::NOP, 0x7);
		} else {
			// SE/VR
			REL::safe_fill(load3DTarget.address() - 0x11, REL::NOP, 0x7);
		}
		stl::write_thunk_call<GetBodyPartDataHook>(load3DTarget.address());
		// TODO: May need to hook other areas?
		logger::info("GetBodyPartData hooked at address {:x}", load3DTarget.
address());
		logger::info("GetBodyPartData hooked at offset {:x}", load3DTarget.o
ffset());
	}
};
struct GetBaseMoveTypes
{
	static RE::BGSMovementType* thunk(RE::TESRace* a_actor, std::uint64_t
 a_type)
	{
		// a_npc WAS the race, but we kept it as Actor for our purposes >:)
		auto actor = reinterpret_cast<RE::Actor*>(a_actor);
		auto appearance = NPCAppearance::GetNPCAppearance(actor->GetActorBas
e());
		if (appearance && appearance->isNPCSwapped) {
			return appearance->alteredNPCData.race->baseMoveTypes[a_type];
		}
		return func(actor->GetActorRuntimeData().race, a_type);
	}
	static inline REL::Relocation<decltype(thunk)> func;
	// Install our hook at the specified address
	static inline void Install()
	{
		// TODO: Hook usages of 140386D90 (1.5.97) for race swapping
		// Mainly for swaps to creatures
	}
};
struct GetFaceRelatedDataHook
{
	static RE::TESRace::FaceRelatedData* GetFaceData(RE::TESNPC* a_npc)
	{
		NPCAppearance* appearance = NPCAppearance::GetNPCAppearance(a_npc);
		if (appearance != nullptr && appearance->isNPCSwapped) {
			return appearance->alteredNPCData.faceRelatedData;
		}
		// Original logic
		return a_npc->race->faceRelatedData[a_npc->GetSex()];
	}
	// Install our hook at the specified address
	static inline void Install()
	{
		REL::Relocation<std::uintptr_t> target{ RELOCATION_ID(24226, 24730),
 REL::VariantOffset(0x8B, 0x5A, 0x8B) };
		REL::safe_fill(target.address(), REL::NOP, 0x10); 
 // Fill with NOP
		if (REL::Module::IsAE()) {
			std::byte newInstructions[] = { (std::byte)0x49, (std::byte)0x89, (
std::byte)0xC6 }; // mov r14, rax
			REL::safe_write(target.address() + 0x5, newInstructions, 3);
		} else {
			std::byte newInstructions[] = { (std::byte)0x48, (std::byte)0x89, (
std::byte)0xC3 }; // mov rbx, rax
			REL::safe_write(target.address() + 0x5, newInstructions, 3);
		}
		auto& trampoline = SKSE::GetTrampoline();
		SKSE::AllocTrampoline(14);
		trampoline.write_call<5>(target.address(), reinterpret_cast<uintptr_
t>(GetFaceData));
		logger::info("GetFaceRelatedDataHook hooked at address {:x}", target
.address());
		logger::info("GetFaceRelatedDataHook hooked at offset {:x}", target.
offset());
	}
};
struct GetFaceRelatedDataHook2
{
	static std::uint64_t thunk(std::uint64_t a_unk, std::uint64_t a_unk1,
 std::uint64_t a_unk2, RE::TESNPC* a_npc)
	{
		// Swap faceRelatedData for the duration of this function call
		auto oldFaceRelatedData = a_npc->race->faceRelatedData[a_npc->GetSex
()];
		NPCAppearance* appearance = NPCAppearance::GetNPCAppearance(a_npc);
		if (appearance != nullptr && appearance->isNPCSwapped) {
			auto faceRelatedData = appearance->alteredNPCData.faceRelatedData;
			a_npc->race->faceRelatedData[a_npc->GetSex()] = faceRelatedData;
		}
		auto result = func(a_unk, a_unk1, a_unk2, a_npc);
		a_npc->race->faceRelatedData[a_npc->GetSex()] = oldFaceRelatedData;
		return result;
	}
	static inline REL::Relocation<decltype(thunk)> func;
	// Install our hook at the specified address
	static inline void Install()
	{
		// TODO: The function is used elsewhere, may need to hook that too?
		REL::Relocation<std::uintptr_t> target{ RELOCATION_ID(26258, 26837),
 REL::VariantOffset(0x95, 0x7D, 0x95) };
		stl::write_thunk_call<GetFaceRelatedDataHook2>(target.address());
		logger::info("GetFaceRelatedDataHook2 hooked at address {:x}", targe
t.address());
		logger::info("GetFaceRelatedDataHook2 hooked at offset {:x}", target
.offset());
	}
};
// TODO: Consider if this hook should be a separate mod to allow armor
s to load differently
struct AttachTESObjectARMOHook
{
	using BipedObjectSlot = stl::enumeration<RE::BGSBipedObjectForm::Bipe
dObjectSlot, std::uint32_t>;
	// Overwrite TESObjectARMO::AttachToBiped functionality. This hook wi
ll let us load the armor with
	// the closest valid race
	static void AttachToBiped(RE::TESObjectARMO* a_armor, RE::TESRace* a_
race, RE::BipedAnim** a_anim, bool isFemale)
	{
		RE::TESRace* race = utils::GetValidRaceForArmorRecursive(a_armor, a_
race);
		if (!race) {
			// Race can’t load anything from this armor
			logger::warn("Race {} {:x} cannot load armor {} {:x}",
				utils::GetFormEditorID(a_race), a_race->formID,
				utils::GetFormEditorID(a_armor), a_armor->formID);
			return;
		}
		if (armorSlotMap.contains(a_armor)) {
			a_armor->bipedModelData.bipedObjectSlots = armorSlotMap.at(a_armor)
;
		} else {
			armorSlotMap.emplace(a_armor, a_armor->bipedModelData.bipedObjectSl
ots);
		}
		auto origSlots = a_armor->bipedModelData.bipedObjectSlots;
		a_armor->bipedModelData.bipedObjectSlots = GetCorrectBipedSlots(a_ar
mor, race);
		logger::debug("Loading {:x} as race {} {:x} with new slots {:x}, old
 slots {:x}",
			a_armor->formID,
			utils::GetFormEditorID(race),
			race->formID,
			a_armor->bipedModelData.bipedObjectSlots.underlying(),
			origSlots.underlying());
		for (auto addon : a_armor->armorAddons) {
			if (addon->race == race || utils::is_amongst(addon->additionalRaces
, race)) {
				AddToBiped(addon, a_armor, a_anim, isFemale);
			}
		}
		// TODO: Revert somehow? For now, use a cache to store the slots
		// This has the bug of player inventory potentially showing inaccura
te icon for armor
		//a_armor->bipedModelData.bipedObjectSlots = origSlots;
	}
	// Take the armor’s biped slots, and remove the slots that no valid a
ddon for the race supports
	static stl::enumeration<RE::BGSBipedObjectForm::BipedObjectSlot, std:
:uint32_t> GetCorrectBipedSlots(RE::TESObjectARMO* a_armor, RE::TESRac
e* a_race)
	{
		BipedObjectSlot addonSlots = RE::BGSBipedObjectForm::BipedObjectSlot
::kNone;
		BipedObjectSlot armorSlots = a_armor->bipedModelData.bipedObjectSlot
s;
		for (auto addon : a_armor->armorAddons) {
			if (addon->race == a_race || utils::is_amongst(addon->additionalRac
es, a_race)) {
				addonSlots |= addon->bipedModelData.bipedObjectSlots;
			}
		}
		return armorSlots & addonSlots;
	}
	static void AddToBiped(RE::TESObjectARMA* a_addon, RE::TESObjectARMO*
 a_armor, RE::BipedAnim** a_anim, bool isFemale) {
		addToBiped(a_addon, a_armor, a_anim, isFemale);
	}
	// TESObjectARMO::AddToBiped(...)
	static inline REL::Relocation<decltype(AddToBiped)> addToBiped;
	static inline std::map<RE::TESObjectARMO*, BipedObjectSlot> armorSlot
Map;
	// Install our hook at the specified address
	static inline void Install()
	{
		REL::Relocation<std::uintptr_t> target{ RELOCATION_ID(17392, 17792) 
};
		addToBiped = { RELOCATION_ID(17361, 17759) };
		auto& trampoline = SKSE::GetTrampoline();
		SKSE::AllocTrampoline(14);
		trampoline.write_branch<5>(target.address(), AttachToBiped);
		logger::info("AttachTESObjectARMOHook hooked at address {:x}", targe
t.address());
		logger::info("AttachTESObjectARMOHook hooked at offset {:x}", target
.offset());
	}
};
struct LoadTESObjectARMOHook
{
	// We swap the race being passed to be what the NPC’s new appearance 
is
	static std::uint64_t thunk(RE::TESObjectARMO* a_armor, RE::TESRace* a
_race, RE::BipedAnim** a_anim, bool isFemale)
	{
		auto race = a_race;
		auto NPC = (*a_anim)->actorRef.get().get()->As<RE::Actor>()->GetActo
rBase();
		logger::debug("LoadTESObjectARMOHook: Loading {} {:x} for NPC {} {:x
}",
			utils::GetFormEditorID(a_armor), a_armor->formID,
			utils::GetFormEditorID(NPC), NPC->formID);
		NPCAppearance* appearance = NPCAppearance::GetNPCAppearance(NPC);
		if (appearance != nullptr && appearance->isNPCSwapped) {
			// Swap to new appearance’s race
			race = appearance->alteredNPCData.race;
		}
		return func(a_armor, race, a_anim, isFemale);
	}
	// TESObjectARMO::AddToBiped(...)
	static inline REL::Relocation<decltype(thunk)> func;
	// Install our hook at the specified address
	static inline void Install()
	{
		REL::Relocation<std::uintptr_t> target{ RELOCATION_ID(24232, 24736),
 REL::VariantOffset(0x302, 0x302, 0x302) };
		stl::write_thunk_call<LoadTESObjectARMOHook>(target.address());
		REL::Relocation<std::uintptr_t> target2{ RELOCATION_ID(24233, 24737)
, REL::VariantOffset(0x78, 0x78, 0x78) };
		stl::write_thunk_call<LoadTESObjectARMOHook>(target2.address());
		REL::Relocation<std::uintptr_t> target3{ RELOCATION_ID(24237, 24741)
, REL::VariantOffset(0xEE, 0xEE, 0xEE) };
		stl::write_thunk_call<LoadTESObjectARMOHook>(target3.address());
		logger::info("LoadTESObjectARMOHook hooked at address {:x}", target.
address());
		logger::info("LoadTESObjectARMOHook hooked at offset {:x}", target.o
ffset());
		logger::info("LoadTESObjectARMOHook hooked at address {:x}", target2
.address());
		logger::info("LoadTESObjectARMOHook hooked at offset {:x}", target2.
offset());
		logger::info("LoadTESObjectARMOHook hooked at address {:x}", target3
.address());
		logger::info("LoadTESObjectARMOHook hooked at offset {:x}", target3.
offset());
	}
};
struct LoadSkinHook
{
	// Based off 1.5.97 TESObjectARMA::ContainsRace_140226D70(v9[i], a_ra
ce)
	// We swap the race being passed to be what the NPC’s new appearance 
is
	// We also swap the armor skin to make sure it is correct
	static std::uint64_t thunk(RE::TESObjectARMO* a_skin, RE::TESRace* a_
race, RE::BipedAnim** a_anim, bool isFemale)
	{
		auto race = a_race;
		auto NPC = (*a_anim)->actorRef.get().get()->As<RE::Actor>()->GetActo
rBase();
		auto skin = a_skin;
		NPCAppearance* appearance = ApplyAppearanceToNPC(NPC);
		if (appearance != nullptr && appearance->isNPCSwapped) {
			// Swap to new appearance’s race and skin
			race = appearance->alteredNPCData.race;
			skin = appearance->alteredNPCData.skin;
			logger::debug("LoadSkinHook: Swap occurred!");
		}
		logger::debug("LoadSkinHook: Loading {} {:x} for NPC {} {:x}",
			utils::GetFormEditorID(skin).c_str(), skin->formID,
			utils::GetFormEditorID(NPC).c_str(), NPC->formID);
		return func(skin, race, a_anim, isFemale);
	}
	// This hook seems to be the best earliest hook spot for applying app
earance as the NPC loads
	static NPCAppearance* ApplyAppearanceToNPC(RE::TESNPC* a_npc)
	{
		logger::debug("Loading {:x} appearance from LoadSkinHook hook", a_np
c->formID);
		auto appearance = NPCAppearance::GetOrCreateNPCAppearance(a_npc);
		if (appearance && !appearance->isNPCSwapped) {
			appearance->ApplyNewAppearance(false);
		}
		return appearance;
	}
	// TESObjectARMO::AddToBiped(...)
	static inline REL::Relocation<decltype(thunk)> func;
	// Install our hook at the specified address
	static inline void Install()
	{
		REL::Relocation<std::uintptr_t> target{ RELOCATION_ID(15499, 15676),
 REL::VariantOffset(0x173, 0x359, 0x173) };
		stl::write_thunk_call<LoadSkinHook>(target.address());
		logger::info("LoadSkinHook hooked at address {:x}", target.address()
);
		logger::info("LoadSkinHook hooked at offset {:x}", target.offset());
	}
};
struct PopulateGraphHook
{
	static std::uint64_t thunk(RE::Actor* a_actor, std::uint64_t a_unk1, 
std::uint64_t a_unk2)
	{
		auto appearance = NPCAppearance::GetNPCAppearance(a_actor->GetActorB
ase());
		auto origRace = a_actor->GetActorRuntimeData().race;
		if (appearance && appearance->isNPCSwapped) {
			a_actor->GetActorBase()->race = appearance->alteredNPCData.race;
		}
		auto result = func(a_actor, a_unk1, a_unk2);
		if (appearance && appearance->isNPCSwapped) {
			a_actor->GetActorBase()->race = origRace;
		}
		return result;
	}
	static inline REL::Relocation<decltype(thunk)> func;
	static inline std::uint32_t idx = 0x72;
	// Install our hook at the specified address
	static inline void Install()
	{
		stl::write_vfunc<RE::Character, PopulateGraphHook>();
		logger::info("PopulateGraphHook hooked!");
	}
};
struct CopyFromTemplate
{
	static void thunk(RE::TESActorBaseData* a_self, RE::TESActorBase* a_t
emplate)
	{
		auto NPC = skyrim_cast<RE::TESNPC*, RE::TESActorBaseData>(a_self);
		auto templateNPC = skyrim_cast<RE::TESNPC*, RE::TESActorBase>(a_temp
late);
		if (!NPC || !templateNPC) {
			return func(a_self, a_template);
		}
		// Remove any existing appearance data
		// Since this NPC is being copied from a template, we are treating t
his NPC as brand new
		NPCAppearance::EraseNPCAppearance(NPC);
		func(a_self, a_template);
		// Process base NPC now that it grabbed the new template data
		ProcessBaseNPC(NPC);
		return;
	}
	static void ProcessBaseNPC(RE::TESNPC* NPC) {
		auto NPCAppearance = NPCAppearance::GetOrCreateNPCAppearance(NPC);
		if (NPCAppearance) {
			NPCAppearance->ApplyNewAppearance(false);
		}
	}
	static inline REL::Relocation<decltype(thunk)> func;
	static inline std::uint32_t idx = 0x4;
	// Install our hook at the specified address
	static inline void Install()
	{
		stl::write_vfunc<RE::TESNPC, 1, CopyFromTemplate>();
		logger::info("CopyFromTemplate hook set");
	}
};
struct CopyNPC
{
	// Maintain Swapper data when copying data between NPCs
	static void thunk(RE::TESNPC* a_self, RE::TESForm* a_other)
	{
		if (!a_other->As<RE::TESNPC>()) {
			func(a_self, a_other); // This should just NOP, but invoke to be sa
fe
			return;
		}
		bool otherNPCSwapped = false;
		NPCAppearance* otherNPCAppearance = NPCAppearance::GetNPCAppearance(
a_other->As<RE::TESNPC>());
		if (otherNPCAppearance) {
			// Swapper data existed for other NPC, revert to original appearanc
e for the copy
			if (otherNPCAppearance->isNPCSwapped) {
				otherNPCAppearance->RevertNewAppearance(false);
				otherNPCSwapped = true;
			}
		}
		func(a_self, a_other);
		// Erase Swapper data, and set it up to match other Swapper data if 
it exists
		NPCAppearance::EraseNPCAppearance(a_self);
		if (otherNPCAppearance != nullptr) {
			// This should get the exact same appearance as the old NPC
			// No user would realistically specify a template/non-unique NPC fo
r an entry, 
			// so the new appearance data should be the same as the old one 
			NPCAppearance::GetOrCreateNPCAppearance(a_self); 
			if (otherNPCSwapped && NPCAppearance::GetNPCAppearance(a_self)) {
				NPCAppearance::GetNPCAppearance(a_self)->ApplyNewAppearance(false)
;
			}
		}
		// Reapply swap to other NPC if necessary
		if (otherNPCAppearance && otherNPCSwapped) {
			otherNPCAppearance->ApplyNewAppearance(false);
		}	
	}
	static inline REL::Relocation<decltype(thunk)> func;
	static inline std::uint32_t idx = 0x2F;
	// Install our hook at the specified address
	static inline void Install()
	{
		stl::write_vfunc<RE::TESNPC, 0, CopyNPC>();
		logger::info("CopyNPC hook set");
	}
};
struct DtorNPC
{
	// Remove Swapper data when NPC being cleared
	static void thunk(RE::TESNPC* a_self, std::byte unk)
	{
		NPCAppearance::EraseNPCAppearance(a_self);
		func(a_self, unk);
	}
	static inline REL::Relocation<decltype(thunk)> func;
	static inline std::uint32_t idx = 0;
	// Install our hook at the specified address
	static inline void Install()
	{
		stl::write_vfunc<RE::TESNPC, 0, DtorNPC>();
		logger::info("DtorNPC hook set");
	}
};
struct SaveNPC
{
	// Revert any swaps before saving to prevent presistence
	static void thunk(RE::TESNPC* a_self, std::uint64_t unkSaveStruct)
	{
		auto appearance = NPCAppearance::GetNPCAppearance(a_self);
		if (!appearance) {
			// No appearance data means no need to revert anything
			return;
		}
		bool appliedSwap = appearance->isNPCSwapped;
		if (appearance && appliedSwap) {
			logger::info("Reverting NPC for save: {}", a_self->formID);
			appearance->RevertNewAppearance(false);
		}
		func(a_self, unkSaveStruct);
		if (appearance && appliedSwap) {
			appearance->ApplyNewAppearance(false);
		}
	}
	static inline REL::Relocation<decltype(thunk)> func;
	static inline std::uint32_t idx = 0xE;
	// Install our hook at the specified address
	static inline void Install()
	{
		stl::write_vfunc<RE::TESNPC, 0, SaveNPC>();
		logger::info("SaveNPC hook set");
	}
};
class HandleFormDelete : public RE::BSTEventSink<RE::TESFormDeleteEven
t>
{
	RE::BSEventNotifyControl ProcessEvent(const RE::TESFormDeleteEvent* a
_event, RE::BSTEventSource<RE::TESFormDeleteEvent>* a_eventSource) ove
rride
	{
		NPCAppearance::EraseNPCAppearance(a_event->formID);
		return RE::BSEventNotifyControl::kContinue;
	}
};
void hook::InstallHooks()
{
	GetTESModelHook::Install();
	GetFaceRelatedDataHook::Install();
	GetFaceRelatedDataHook2::Install();
	GetBodyPartDataHook::Install();
	GetBaseMoveTypes::Install();
	LoadTESObjectARMOHook::Install();
	AttachTESObjectARMOHook::Install();
	LoadSkinHook::Install();
	PopulateGraphHook::Install();
	RE::ScriptEventSourceHolder::GetSingleton()->AddEventSink(new HandleF
ormDelete());
	CopyFromTemplate::Install();
	CopyNPC::Install();
	DtorNPC::Install();
	SaveNPC::Install();
}
//========================================End Hooks.cpp
Begin Hooks.h:
#pragma once
namespace hook
{
	void InstallHooks();
}
//========================================End Hooks.h
Begin main.cpp:
#include "configuration/Configuration.h"
#include "Hooks.h"
#include "Utils.h"
#include "settings/Settings.h"
#include "swap/RaceSwapDatabase.h"
#include "Papyrus.h"
void MessageInterface(SKSE::MessagingInterface::Message* msg) {
	switch (msg->type) {
		case SKSE::MessagingInterface::kPostLoad:
		{
			logger::info("Dependencies check...");
			if (!GetModuleHandle(L"po3_Tweaks")) {
				logger::critical("po3_Tweaks not detected, mod will not function r
ight!");
			}
			logger::info("Dependencies check complete!");
			Settings::GetSingleton()->Load();
			break;
		}
		case SKSE::MessagingInterface::kDataLoaded:
		{
			// TODO: Add console commands to revert appearance
			ConfigurationDatabase::GetSingleton()->Initialize();
			hook::InstallHooks();
			break;
		}
	}
}
void InitializeLog()
{
	auto path = logger::log_directory();
	if (!path) {
		stl::report_and_fail("Failed to find standard logging directory"sv);
	}
	*path /= Version::PROJECT;
	*path += ".log"sv;
	auto sink = std::make_shared<spdlog::sinks::basic_file_sink_mt>(path-
>string(), true);
	auto log = std::make_shared<spdlog::logger>("global log"s, std::move(
sink));
#ifdef _DEBUG
	log->set_level(spdlog::level::debug);
	log->flush_on(spdlog::level::debug);
#else
	log->set_level(spdlog::level::info);
	log->flush_on(spdlog::level::info);
#endif
	spdlog::set_default_logger(std::move(log));
	spdlog::set_pattern("[%H:%M:%S:%e] %v"s);
	logger::info(FMT_STRING("{} v{}"), Version::PROJECT, Version::NAME);
}
extern "C" DLLEXPORT constinit auto SKSEPlugin_Version = []() {
	SKSE::PluginVersionData v;
	v.PluginVersion(Version::MAJOR);
	v.PluginName("SKSE_SBP_Cloning");
	v.UsesAddressLibrary(true);
	v.HasNoStructUse(true);
	return v;
}();
extern "C" DLLEXPORT bool SKSEAPI SKSEPlugin_Query(const SKSE::QueryIn
terface* a_skse, SKSE::PluginInfo* a_info)
{
	a_info->infoVersion = SKSE::PluginInfo::kVersion;
	a_info->name = Version::PROJECT.data();
	a_info->version = Version::MAJOR;
	return true;
}
extern "C" DLLEXPORT bool SKSEAPI SKSEPlugin_Load(const SKSE::LoadInte
rface* a_skse)
{
	InitializeLog();
	SKSE::Init(a_skse);
	auto messaging = SKSE::GetMessagingInterface();
	messaging->RegisterListener(MessageInterface);
	SKSE::GetPapyrusInterface()->Register(Papyrus::Bind);
	logger::info("Loaded Plugin");
	return true;
}
extern "C" DLLEXPORT const char* APIENTRY GetPluginVersion()
{
	return Version::NAME.data();
}
//========================================End main.cpp
Begin NPCAppearance.cpp:
#pragma once
#include "NPCAppearance.h"
#include "configuration/Configuration.h"
#include "NPCSwap.h"
#include "RaceSwap.h"
#include "Utils.h"
static void UpdateLoadedActors(RE::TESNPC* a_npc) {
	// TODO: Update loaded actors
}
bool NPCAppearance::ApplyNewAppearance(bool updateLoadedActors)
{
	if (isNPCSwapped) {
		return false;
	}
	ApplyAppearance(&alteredNPCData);
	UpdateLoadedActors(npc);
	isNPCSwapped = true;
	return true;
}
bool NPCAppearance::RevertNewAppearance(bool updateLoadedActors) {
	if (!isNPCSwapped) {
		return false;
	}
	ApplyAppearance(&originalNPCData);
	UpdateLoadedActors(npc);
	isNPCSwapped = false;
	return true;
}
void NPCAppearance::ApplyAppearance(NPCData* a_data)
{
	if (a_data->isBeastRace && !npc->HasKeywordID(constants::Keyword_IsBe
astRace)) {
		npc->AddKeyword(RE::TESForm::LookupByID(constants::Keyword_IsBeastRa
ce)->As<RE::BGSKeyword>());
	} else {
		// TODO: Can’t remove beast keyword from race for NPC without hook?
	}
	npc->height = a_data->height;
	npc->weight = a_data->weight;
	if (a_data->isFemale) {
		npc->actorData.actorBaseFlags.set(RE::ACTOR_BASE_DATA::Flag::kFemale
);
	} else {
		npc->actorData.actorBaseFlags.reset(RE::ACTOR_BASE_DATA::Flag::kFema
le);
	}
	npc->bodyTintColor = a_data->bodyTintColor;
	npc->skin = a_data->skin;
	npc->farSkin = a_data->farSkin;
	// skeletonModel applied from hooks for race
	// faceRelatedData applied from hooks for race
	if (npc->tintLayers) {
		npc->tintLayers->clear();
	}
	npc->tintLayers = utils::CopyTintLayers(a_data->tintLayers);
	npc->faceNPC = a_data->faceNPC;
	if (npc->faceNPC == npc) {
		npc->faceNPC = nullptr;
	}
	npc->headRelatedData = utils::CopyHeadRelatedData(a_data->headRelated
Data);
	npc->numHeadParts = a_data->numHeadParts;
	npc->headParts = utils::CopyHeadParts(a_data->headParts, a_data->numH
eadParts);
	// TODO: Check for default face struct here?
	npc->faceData = utils::DeepCopyFaceData(a_data->faceData);
}
void NPCAppearance::InitializeNPCData(NPCData* a_data)
{
	a_data->baseNPC = npc;
	a_data->faceNPC = npc->faceNPC ? npc->faceNPC : npc;
	a_data->race = npc->race;
	a_data->skin = npc->skin ? npc->skin : npc->race->skin;
	a_data->farSkin = npc->farSkin;
	a_data->weight = npc->weight;
	a_data->height = npc->height;
	a_data->isFemale = npc->IsFemale();
	a_data->bodyTintColor = npc->bodyTintColor;
	a_data->tintLayers = utils::CopyTintLayers(npc->tintLayers);
	CopyFaceData(a_data);
	a_data->skeletonModel = &npc->race->skeletonModels[npc->GetSex()];
	a_data->isBeastRace = npc->HasKeywordID(constants::Keyword_IsBeastRac
e) ||
	 npc->race->HasKeywordID(constants::Keyword_I
sBeastRace);
	a_data->bodyPartData = npc->race->bodyPartData;
	a_data->bodyTextureModel = &npc->race->bodyTextureModels[npc->GetSex(
)];
	a_data->behaviorGraph = &npc->race->behaviorGraphs[npc->GetSex()];
}
void NPCAppearance::CopyFaceData(NPCData* a_data)
{
	auto memoryManager = RE::MemoryManager::GetSingleton();
	if (a_data->headRelatedData) {
		memoryManager->Deallocate(a_data->headRelatedData, 0);
	}
	a_data->headRelatedData = utils::CopyHeadRelatedData(a_data->faceNPC-
>headRelatedData);
	a_data->numHeadParts = a_data->faceNPC->numHeadParts;
	a_data->headParts = utils::CopyHeadParts(a_data->faceNPC->headParts, 
a_data->faceNPC->numHeadParts);
	// TODO: Check for default face struct here?
	a_data->faceData = utils::DeepCopyFaceData(a_data->faceNPC->faceData)
;
	a_data->faceRelatedData = a_data->faceNPC->race->faceRelatedData[npc-
>GetSex()];
}
void NPCAppearance::SetupNewAppearance() {
	RaceSwap::applySwap(&alteredNPCData, config->otherRace);
	NPCSwap::applySwap(&alteredNPCData, config->otherNPC);
	// TODO add more swaps here
}
NPCAppearance::NPCAppearance(RE::TESNPC* a_npc, AppearanceConfiguratio
n* a_config)
{
	this->npc = a_npc;
	this->config = a_config;
	logger::info("	Creating new NPC data");
	InitializeNPCData(&this->originalNPCData);
	InitializeNPCData(&this->alteredNPCData);
	logger::info("	Setting up appearance");
	SetupNewAppearance();
}
void NPCAppearance::dtor() {
	// TODO: Clear/delete the data inside like tint layers, head data, et
c.?
}
// Filter for only NPCs this swapping can work on
static bool IsNPCValid(RE::TESNPC* a_npc)
{
	return a_npc && a_npc->race &&
	 !a_npc->IsPlayer() &&
	 !a_npc->IsPreset() /* &&
	 a_npc->race->HasKeywordID(constants::Keyword_ActorTypeNPC) */;
}
// Gets or create a new NPC appearance. Will be null if NPC has no alt
ered appearance to take
NPCAppearance* NPCAppearance::GetOrCreateNPCAppearance(RE::TESNPC* a_n
pc) {
	if (!IsNPCValid(a_npc)) {
		return nullptr;
	}
	appearanceMapLock.lock();
	if (appearanceMap.contains(a_npc->formID)) {
		appearanceMapLock.unlock();
		return appearanceMap.at(a_npc->formID);
	}
	auto config = ConfigurationDatabase::GetSingleton()->GetConfiguration
ForNPC(a_npc);
	if (config == nullptr) {
		appearanceMapLock.unlock();
		return nullptr;
	}
	logger::info("Creating new appearance for {:x}", a_npc->formID);
	NPCAppearance* appearance = new NPCAppearance(a_npc, config);
	appearanceMap.insert(std::pair(a_npc->formID, appearance));
	appearanceMapLock.unlock();
	return appearance;
};
NPCAppearance* NPCAppearance::GetNPCAppearance(RE::TESNPC* a_npc) {
	appearanceMapLock.lock();
	if (appearanceMap.contains(a_npc->formID)) {
		appearanceMapLock.unlock();
		return appearanceMap.at(a_npc->formID);
	}
	appearanceMapLock.unlock();
	return nullptr;
};
void NPCAppearance::EraseNPCAppearance(RE::TESNPC* a_npc) {
	EraseNPCAppearance(a_npc->formID);
};
void NPCAppearance::EraseNPCAppearance(RE::FormID a_formID)
{
	appearanceMapLock.lock();
	if (appearanceMap.contains(a_formID)) {
		appearanceMap.at(a_formID)->dtor();
		appearanceMap.erase(a_formID);
	}
	appearanceMapLock.unlock();
};
// Native Papyrus function version of enable
static void ObjectReference__Enable(RE::TESObjectREFR* a_self, bool a_
abFadeIn, bool a_wait, RE::BSScript::Internal::VirtualMachine* a_vm, R
E::VMStackID a_stackID)
{
	using func_t = decltype(&ObjectReference__Enable);
	REL::Relocation<func_t> func{ RELOCATION_ID(56038, 56158) };
	return func(a_self, a_abFadeIn, a_wait, a_vm, a_stackID);
}
void ResetCharacter(RE::Character* a_refr)
{
	if (a_refr == nullptr) {
		return;
	}
	RE::ObjectRefHandle origParentHandle;
	RE::ExtraEnableStateParent* enableStateParent = nullptr;
	// Remove enable state parent temporarily if it exists, so we can dis
able/enable freely to refresh the NPC
	enableStateParent = a_refr->extraList.GetByType<RE::ExtraEnableStateP
arent>();
	if (enableStateParent) {
		origParentHandle = enableStateParent->parent;
		enableStateParent->parent = RE::ObjectRefHandle();
	}
	a_refr->Disable();
	ObjectReference__Enable(a_refr, false, false, RE::BSScript::Internal:
:VirtualMachine::GetSingleton(), 0);
	if (enableStateParent) {
		enableStateParent->parent = origParentHandle;
	}
}
//========================================End NPCAppearance.cpp
Begin NPCAppearance.h:
#pragma once
#include "configuration/Configuration.h"
class NPCAppearance
{
public:
	using pad = std::byte;
	// Appearance information of an NPC, used to swap and revert
	// Some variables are specifically padded so we can call TESNPC funct
ions
	// with this data, such as "TESNPC::ChangeHeadPart", and treat this d
ata
	// as the TESNPC
	struct NPCData
	{
		RE::TESNPC* baseNPC; // 00
		RE::TESModel* skeletonModel; // 08
		bool isFemale; // 09
		////////////////// RACE DATA /////////////////////////////
		bool isBeastRace; // 0A
		RE::TESObjectARMO* skin; // 10
		RE::TESRace::FaceRelatedData* faceRelatedData; // 18
		RE::BGSBodyPartData* bodyPartData; // 20
		RE::BGSTextureModel* bodyTextureModel; // 28
		RE::BGSBehaviorGraphModel* behaviorGraph; // 30
		////////////////////////////////////////////////////
		pad pad18[0x188]; // 38
		RE::TESNPC::HeadRelatedData* headRelatedData; // 1C8
		pad pad1D0[0x18]; // 1D0
		RE::TESRace* race; // 1E8 - originalRace
		RE::TESNPC* faceNPC; // 1F0
		float height; // 1F8
		float weight; // 1FC
		pad pad200[0x10]; // 200
		RE::TESObjectARMO* farSkin; // 210
		pad pad218[0x20]; // 218
		RE::BGSHeadPart** headParts; // 238
		std::uint8_t numHeadParts; // 240
		pad pad241[0x5]; // 241
		RE::Color bodyTintColor; // 246
		pad pad247[0xE]; // 24A
		RE::TESNPC::FaceData* faceData; // 258
		RE::BSTArray<RE::TESNPC::Layer*>* tintLayers; // 260
	};
	static_assert(sizeof(NPCData) == 0x268);
	static_assert(offsetof(NPCData, faceData) == 0x258);
	static_assert(offsetof(NPCData, tintLayers) == 0x260);
	static_assert(offsetof(NPCData, headParts) == 0x238);
	static_assert(offsetof(NPCData, numHeadParts) == 0x240);
	NPCData originalNPCData = { 0 };
	NPCData alteredNPCData = { 0 };
	bool isNPCSwapped = false;
	bool ApplyNewAppearance(bool updateLoadedActors);
	bool RevertNewAppearance(bool updateLoadedActors);
	static NPCAppearance* GetOrCreateNPCAppearance(RE::TESNPC* a_npc);
	static NPCAppearance* GetNPCAppearance(RE::TESNPC* a_npc);
	static void EraseNPCAppearance(RE::TESNPC* a_npc);
	static void EraseNPCAppearance(RE::FormID a_formID);
private:
	RE::TESNPC* npc;
	AppearanceConfiguration* config;
	NPCAppearance(RE::TESNPC* a_npc, AppearanceConfiguration* a_config);
	void InitializeNPCData(NPCData* a_data);
	void SetupNewAppearance();
	void CopyFaceData(NPCData* a_data);
	void ApplyAppearance(NPCData* a_data);
	void dtor();
	static inline std::recursive_mutex appearanceMapLock;
	static inline std::map<RE::FormID, NPCAppearance*> appearanceMap;
};
// TODO: This is here to ensure size error in CLIB-NG fails this build
 unless corrected
// For some reason, this is needed to correctly store and use layers f
or this mod
//static_assert(sizeof(RE::TESNPC::Layer) == 0xC);
//========================================End NPCAppearance.h
Begin NPCSwap.cpp:
#pragma once
#include "NPCSwap.h"
#include "Utils.h"
void NPCSwap::applySwap(NPCAppearance::NPCData* a_data, RE::TESNPC* a_
otherNPC) {
	if (!a_otherNPC || a_data->baseNPC == a_otherNPC) {
		return;
	}
	auto otherNPCAppearance = NPCAppearance::GetNPCAppearance(a_otherNPC)
;
	auto otherNPCSwapped = false;
	if (otherNPCAppearance && otherNPCAppearance->isNPCSwapped) {
		// Keep original so we can apply original NPC data
		otherNPCSwapped = true;
		NPCAppearance::GetNPCAppearance(a_otherNPC)->RevertNewAppearance(fal
se);
	}
	a_data->baseNPC = a_otherNPC;
	a_data->isBeastRace = a_otherNPC->HasKeywordID(constants::Keyword_IsB
eastRace) ||
	 a_otherNPC->race->HasKeywordID(constants::Keywo
rd_IsBeastRace);
	a_data->height = a_otherNPC->height;
	a_data->weight = a_otherNPC->weight;
	a_data->isFemale = a_otherNPC->IsFemale();
	a_data->bodyTintColor = a_otherNPC->bodyTintColor;
	a_data->skin = a_otherNPC->skin ? a_otherNPC->skin : a_otherNPC->race
->skin;
	a_data->farSkin = a_otherNPC->farSkin;
	a_data->race = a_otherNPC->race;
	a_data->skeletonModel = &a_otherNPC->race->skeletonModels[a_otherNPC-
>GetSex()];
	a_data->faceRelatedData = a_otherNPC->race->faceRelatedData[a_otherNP
C->GetSex()];
	a_data->bodyPartData = a_otherNPC->race->bodyPartData;
	a_data->bodyTextureModel = &a_otherNPC->race->bodyTextureModels[a_oth
erNPC->GetSex()];
	a_data->behaviorGraph = &a_otherNPC->race->behaviorGraphs[a_otherNPC-
>GetSex()];
	if (a_data->tintLayers) {
		a_data->tintLayers->clear();
	}
	a_data->tintLayers = utils::CopyTintLayers(a_otherNPC->tintLayers);
	a_data->faceNPC = a_otherNPC->faceNPC ? a_otherNPC->faceNPC : a_other
NPC;
	a_data->headRelatedData = utils::CopyHeadRelatedData(a_data->faceNPC-
>headRelatedData);
	a_data->numHeadParts = a_otherNPC->numHeadParts;
	a_data->headParts = utils::CopyHeadParts(a_data->faceNPC->headParts, 
a_data->faceNPC->numHeadParts);
	// TODO: Check for default face struct here?
	a_data->faceData = utils::DeepCopyFaceData(a_data->faceNPC->faceData)
;
	if (otherNPCAppearance && otherNPCSwapped) {
		otherNPCSwapped = true;
		NPCAppearance::GetNPCAppearance(a_otherNPC)->ApplyNewAppearance(fals
e);
	}
	logger::info("Swap complete!");
}
//========================================End NPCSwap.cpp
Begin NPCSwap.h:
#pragma once
#include "NPCAppearance.h"
class NPCSwap
{
public:
	static void applySwap(NPCAppearance::NPCData* a_data, RE::TESNPC* a_o
therNPC);
};
// TODO: This is here to ensure size error in CLIB-NG fails this build
 unless corrected
//static_assert(sizeof(RE::TESNPC::Layer) == 0xC); 
//========================================End NPCSwap.h
Begin Papyrus.cpp:
#include "Papyrus.h"
#include "Cloning.h"
namespace Papyrus
{
	bool Bind(VM* a_vm)
	{
		if (!a_vm) {
			logger::critical("couldn’t get VM State"sv);
			return false;
		}
		BIND(Cloning);
		// Ajoutez d’autres fonctions ˆ  enregistrer ici si nˆ'cessaire
		return true;
	}
}
//========================================End Papyrus.cpp
Begin Papyrus.h:
#pragma once
namespace Papyrus
{
	#define BIND(a_method, ...) a_vm->RegisterFunction(#a_method##sv, scr
ipt, a_method __VA_OPT__(, ) __VA_ARGS__)
	using VM = RE::BSScript::Internal::VirtualMachine;
	using StackID = RE::VMStackID;
	inline constexpr auto script = "SBP"sv;
	bool Bind(VM* a_vm);
}
//========================================End Papyrus.h
Begin PCH.cpp:
namespace stl
{
	namespace detail
	{
		struct asm_patch :
			Xbyak::CodeGenerator
		{
			asm_patch(std::uintptr_t a_dst)
			{
				Xbyak::Label dst;
				mov(rax, a_dst);
				jmp(rax);
			}
		};
	}
	void asm_jump(std::uintptr_t a_from, [[maybe_unused]] std::size_t a_s
ize, std::uintptr_t a_to)
	{
		detail::asm_patch p{ a_to };
		p.ready();
		assert(p.getSize() <= a_size);
		REL::safe_write(
			a_from,
			std::span{ p.getCode<const std::byte*>(), p.getSize() });
	}
	void asm_replace(std::uintptr_t a_from, std::size_t a_size, std::uint
ptr_t a_to)
	{
		REL::safe_fill(a_from, REL::INT3, a_size);
		asm_jump(a_from, a_size, a_to);
	}
}
//========================================End PCH.cpp
Begin PCH.h:
#pragma once
#define NOMMNOSOUND
#include "RE/Skyrim.h"
#include "SKSE/SKSE.h"
#pragma warning(disable: 4100)
#pragma warning(push)
#include <SimpleIni.h>
#include <robin_hood.h>
#include <spdlog/sinks/basic_file_sink.h>
#include <xbyak/xbyak.h>
#pragma warning(pop)
namespace logger = SKSE::log;
using namespace std::literals;
namespace constants
{
	inline RE::FormID Keyword_ActorTypeNPC = 0x13794;
	inline RE::FormID Keyword_IsBeastRace = 0xD61D1;
	// debug
	inline RE::FormID Maiq = 0x954BF;
	inline RE::FormID Nazeem = 0x13BBF;
	inline RE::FormID Urog = 0x1B078;
	inline RE::FormID MQ101Alduin = 0x32B94;
	inline RE::FormID DefaultRace = 0x19;
	inline RE::FormID DebugNPCToTest = 0x954BF;
	inline RE::FormID KhajiitRace = 0x13745;
	inline RE::FormID ArgonianRace = 0x13740;
	inline RE::FormID RedguardRace = 0x13748;
	inline RE::FormID NordRace = 0x13746;
	inline RE::FormID OrcRace = 0x13747;
	inline RE::FormID CowRace = 0x4E785;
	inline RE::FormID DragonRace = 0x12E82;
	inline RE::FormID BretonRace = 0x13741;
}
namespace stl
{
	using namespace SKSE::stl;
	void asm_replace(std::uintptr_t a_from, std::size_t a_size, std::uint
ptr_t a_to);
	template <class T>
	void asm_replace(std::uintptr_t a_from)
	{
		asm_replace(a_from, T::size, reinterpret_cast<std::uintptr_t>(T::fun
c));
	}
	template <class T>
	void write_thunk_call(std::uintptr_t a_src)
	{
		auto& trampoline = SKSE::GetTrampoline();
		SKSE::AllocTrampoline(14);
		T::func = trampoline.write_call<5>(a_src, T::thunk);
	}
	template <class F, size_t offset, class T>
	void write_vfunc()
	{
		REL::Relocation<std::uintptr_t> vtbl{ F::VTABLE[offset] };
		T::func = vtbl.write_vfunc(T::idx, T::thunk);
	}
	template <class F, class T>
	void write_vfunc()
	{
		write_vfunc<F, 0, T>();
	}
	inline std::string as_string(std::string_view a_view)
	{
		return { a_view.data(), a_view.size() };
	}
}
#ifdef SKYRIM_AE
#	define REL_ID(se, ae) REL::ID(ae)
#	define OFFSET(se, ae) ae
#	define OFFSET_3(se, ae, vr) ae
#elif SKYRIMVR
#	define REL_ID(se, ae) REL::ID(se)
#	define OFFSET(se, ae) se
#	define OFFSET_3(se, ae, vr) vr
#else
#	define REL_ID(se, ae) REL::ID(se)
#	define OFFSET(se, ae) se
#	define OFFSET_3(se, ae, vr) se
#endif
#define DLLEXPORT __declspec(dllexport)
#include "Version.h"
//========================================End PCH.h
Begin RaceSwap.cpp:
#include "PCH.h"
#include "RaceSwapUtils.h"
#include "Utils.h"
#include "RaceSwap.h"
#include "RaceSwapDatabase.h"
std::string GetHeadPartTypeAsName(RE::BGSHeadPart::HeadPartType a_type
) {
	if (a_type == RE::BGSHeadPart::HeadPartType::kEyebrows) {
		return "Eyebrows";
	} else if (a_type == RE::BGSHeadPart::HeadPartType::kEyes) {
		return "Eyes";
	} else if (a_type == RE::BGSHeadPart::HeadPartType::kFace) {
		return "Face";
	} else if (a_type == RE::BGSHeadPart::HeadPartType::kFacialHair) {
		return "Facial Hair";
	} else if (a_type == RE::BGSHeadPart::HeadPartType::kHair) {
		return "Hair";
	} else if (a_type == RE::BGSHeadPart::HeadPartType::kMisc) {
		return "Misc";
	} else if (a_type == RE::BGSHeadPart::HeadPartType::kScar) {
		return "Scar";
	}
	return "Unknown";
}
void RaceSwap::applySwap(NPCAppearance::NPCData* a_data, RE::TESRace* 
a_otherRace)
{
	if (!a_otherRace || a_data->race == a_otherRace) {
		// Don’t do anything if no other race present, or NPC is already sai
d race
		return;
	}
	if (a_data->baseNPC->race->IsChildRace() != a_otherRace->IsChildRace(
)) {
		// Don’t allow child NPCs swapping to non-child races and vice versa
		logger::warn("Attempting to swap {:x} to race {} {:x}. Childen canno
t be swapped to non-child races and vice versa!",
			a_data->baseNPC->formID, utils::GetFormEditorID(a_otherRace), a_oth
erRace->formID);
		return;
	}
	logger::info("Swapping {} {:x} to {} {:x}", 
		utils::GetFormEditorID(a_data->baseNPC).c_str(),
		a_data->baseNPC->formID, 
		utils::GetFormEditorID(a_otherRace),
		a_otherRace->formID
	);
	auto originalRace = a_data->race;
	a_data->race = a_otherRace;
	a_data->faceNPC = nullptr; // Prevents cases where face NPC is config
ured differently
	a_data->skeletonModel = &a_otherRace->skeletonModels[a_data->isFemale
];
	a_data->isBeastRace = a_otherRace->HasKeywordID(constants::Keyword_Is
BeastRace);
	a_data->skin = a_otherRace->skin;
	a_data->faceRelatedData = a_otherRace->faceRelatedData[a_data->isFema
le];
	a_data->bodyPartData = a_otherRace->bodyPartData;
	a_data->bodyTextureModel = &a_otherRace->bodyTextureModels[a_data->is
Female];
	a_data->behaviorGraph = &a_otherRace->behaviorGraphs[a_data->isFemale
];
	raceutils::RandomGen rand_generator(a_data->baseNPC);	
	DoHeadData(rand_generator, a_data);
	DoHeadParts(rand_generator, a_data);
	DoTints(rand_generator, a_data, originalRace);
	DoHeadMorphs(rand_generator, a_data);
	return;
}
RE::BGSColorForm* GetClosestColorForm(RE::BGSColorForm* a_colorForm, R
E::BSTArray<RE::BGSColorForm*>* a_colors)
{
	if (!a_colorForm || !a_colors || a_colors->empty()) {
		return nullptr;
	}
	RE::BGSColorForm* closestColor = nullptr;
	int closestPresetMatch = 1000000; // Closer to 0.0 is better
	RE::Color origColor = a_colorForm->color;
	for (auto colorForm : *a_colors) {
		RE::Color currentColor = colorForm->color;
		int currentPresetMatch = std::abs(origColor.blue - currentColor.blue
) +
		 std::abs(origColor.green - currentColor.gre
en) +
		 std::abs(origColor.red - currentColor.red);
		if (currentPresetMatch < closestPresetMatch) {
			closestPresetMatch = currentPresetMatch;
			closestColor = colorForm;
		}
	}
	return closestColor;
}
bool RaceSwap::DoHeadData(raceutils::RandomGen rand_gen, NPCAppearance
::NPCData* a_data)
{
	auto database = raceswap::DataBase::GetSingleton();
	if (!a_data->race->faceRelatedData[a_data->isFemale]) {
		return false;
	}
	auto defaultTexture = a_data->race->faceRelatedData[a_data->isFemale]
->defaultFaceDetailsTextureSet;
	if (!a_data->headRelatedData->faceDetails) {
		if (defaultTexture) {
			logger::debug("No skin texture, using new default {} {:x}", utils::
GetFormEditorID(defaultTexture).c_str(), defaultTexture->formID);
			a_data->headRelatedData->faceDetails = defaultTexture;
		} else {
			logger::debug("No skin texture, using NONE");
			a_data->headRelatedData->faceDetails = nullptr;
		}	
	} else {
		auto item_list = database->GetMatchedSkinTextureResults(static_cast<
RE::SEX>(a_data->isFemale), a_data->race, a_data->headRelatedData->fac
eDetails);
		auto new_item = raceutils::random_pick(item_list, rand_gen.GetNext()
);
		if (new_item) {
			logger::debug("Swapping from {} {:x} to skin texture {} {:x}",
				utils::GetFormEditorID(a_data->headRelatedData->faceDetails).c_str
(), a_data->headRelatedData->faceDetails->formID,
				utils::GetFormEditorID(new_item).c_str(), new_item->formID);
			a_data->headRelatedData->faceDetails = new_item;
		} else {
			logger::debug("New skin texture null, using new default {} {:x}", u
tils::GetFormEditorID(defaultTexture).c_str(), defaultTexture->formID)
;
			a_data->headRelatedData->faceDetails = defaultTexture;
		}
	}
	auto currentHairColor = a_data->headRelatedData->hairColor;
	auto allHairColors = a_data->race->faceRelatedData[a_data->isFemale]-
>availableHairColors;
	a_data->headRelatedData->hairColor = GetClosestColorForm(currentHairC
olor, allHairColors);
	return true;
}
bool RaceSwap::DoHeadParts(raceutils::RandomGen rand_gen, NPCAppearanc
e::NPCData* a_data)
{
	std::vector<RE::BGSHeadPart*> oldHeadparts;
	std::vector<RE::BGSHeadPart*> newHeadParts;
	// First passthrough, gather all non-extra headparts into arraylist
	for (std::uint8_t i = 0; i < a_data->numHeadParts; i++) {
		if (a_data->headParts[i] && !a_data->headParts[i]->IsExtraPart()) {
			oldHeadparts.push_back(a_data->headParts[i]);
		}
	}
	// Second passthrough, swap to other race headparts
	for (auto headpart : oldHeadparts) {
		auto newPart = SwitchHeadPart(rand_gen, a_data, headpart);
		auto oldPart = headpart;
		logger::debug("{:x} Swapping {} {} {:x} to {} {} {:x}",
			a_data->baseNPC->formID,
			GetHeadPartTypeAsName(oldPart->type.get()),
			utils::GetFormEditorID(oldPart),
			oldPart->formID,
			GetHeadPartTypeAsName(newPart->type.get()),
			utils::GetFormEditorID(newPart),
			newPart->formID);
		// Add new headpart, along with its extras
		newHeadParts.push_back(newPart);
		for (auto extra : newPart->extraParts) {
			newHeadParts.push_back(extra);
		}
	}
	// Final passthrough, replace the original headparts with the new one
s
	auto numHeadParts = (std::uint8_t) newHeadParts.size();
	RE::BGSHeadPart** headparts = utils::AllocateMemoryCleanly<RE::BGSHea
dPart*>(numHeadParts * 8);
	for (std::uint8_t i = 0; i < numHeadParts; i++) {
		headparts[i] = newHeadParts.at(i);
	}
	a_data->numHeadParts = numHeadParts;
	a_data->headParts = headparts;
	return true;
}
bool RaceSwap::DoHeadMorphs(raceutils::RandomGen rand_gen, NPCAppearan
ce::NPCData* a_data)
{
	if (!a_data->race->faceRelatedData[a_data->isFemale]) {
		return false;
	}
	// Pick a random preset, and set morphs/parts to match
	auto presetNPCs = a_data->race->faceRelatedData[a_data->isFemale]->pr
esetNPCs;
	if (!presetNPCs || presetNPCs->empty()) {
		logger::info(" No presets available!");
		return false;
	}
	auto newNPC = raceutils::random_pick(
		*presetNPCs, 
		(int) rand_gen.GetNext()
	);
	auto morphs = newNPC->faceData->morphs;
	auto parts = newNPC->faceData->parts;
	logger::debug(" Swapping morphs/parts");
	for (auto i = 0; i < RE::TESNPC::FaceData::Morphs::kTotal; i++) {
		a_data->faceData->morphs[i] = morphs[i];
	}
	for (auto i = 0; i < RE::TESNPC::FaceData::Parts::kTotal; i++) {
		a_data->faceData->parts[i] = parts[i];
	}
	return false;
}
std::uint16_t GetClosestPresetIdx(RE::Color a_color, RE::TESRace::Face
RelatedData::TintAsset::Presets a_presets) {
	std::uint16_t closestPresetIdx = 0;
	int closestPresetMatch = 1000000; // Closer to 0.0 is better
	for (std::uint16_t i = 0; i < a_presets.colors.size(); i++) {
		RE::Color currentColor = a_presets.colors[i]->color;
		int currentPresetMatch = std::abs(a_color.blue - currentColor.blue) 
+
		 std::abs(a_color.green - currentColor.gre
en) +
		 std::abs(a_color.red - currentColor.red);
		if (currentPresetMatch < closestPresetMatch) {
			closestPresetMatch = currentPresetMatch;
			closestPresetIdx = i;
		}
	}
	return closestPresetIdx;
}
bool RaceSwap::DoTints(raceutils::RandomGen rand_gen, NPCAppearance::N
PCData* a_data, RE::TESRace* a_originalRace)
{
	if (!a_originalRace->faceRelatedData[a_data->isFemale] ||
		!a_originalRace->faceRelatedData[a_data->isFemale]->tintMasks) {
		logger::info(" No tint masks for the original race!");
		return false;
	}
	if (!a_data->race->faceRelatedData[a_data->isFemale] ||
		!a_data->race->faceRelatedData[a_data->isFemale]->tintMasks) {
		logger::info(" No tint masks for the new race!");
		return false;
	}
	if (!a_data->tintLayers) {
		// NPC has no tints, but the new race does. Create a tint array to e
nsure skin matches face later
		logger::warn(" {:x} has no tint layers!", a_data->baseNPC->formID);
		a_data->tintLayers = utils::AllocateMemoryCleanly<RE::BSTArray<RE::T
ESNPC::Layer*>>();
	}
	auto skintone_tintasset = raceswap::DataBase::GetSingleton()->GetRace
SkinTint(static_cast<RE::SEX>(a_data->isFemale), a_data->race);
	auto& originalTintAssets = a_originalRace->faceRelatedData[a_data->is
Female]->tintMasks;
	auto& newTintAssets = a_data->race->faceRelatedData[a_data->isFemale]
->tintMasks;
	bool bodyColorFixed = false;
	// Loop through the tints and find equivalent random tints for the ot
her race
	for (auto& tint : *(a_data->tintLayers)) {
		RE::TESRace::FaceRelatedData::TintAsset* originalTintAsset = nullptr
;
		auto matchedTints = RE::BSTArray<RE::TESRace::FaceRelatedData::TintA
sset*>();
		matchedTints.clear();
		// Find original tint asset from the original race
		for (auto& asset : *originalTintAssets) {
			if (asset->texture.index == tint->tintIndex) {
				originalTintAsset = asset;
				break;
			}
		}
		if (!originalTintAsset) {
			logger::debug("tint index {} not found", tint->tintIndex);
			// Can’t find original asset, invalid tint? Remove the tint to be s
afe
			tint->interpolationValue = 0; // TODO: Check if this actually remov
es the tint from being displayed?
			tint->tintIndex = 65535;
			tint->tintColor = RE::Color(255, 255, 255, 255);
			continue;
		}
		// Find all tint assets in new race that is the same type, then pick
 a pseudo-random one
		for (auto& asset : *newTintAssets) {
			if (asset->texture.skinTone == originalTintAsset->texture.skinTone)
 {
				matchedTints.push_back(asset);
			}
		}
		if (matchedTints.empty()) {
			logger::debug("tint index {} does not have matching tints", tint->t
intIndex);
			// Can’t find original asset, invalid tint? Remove the tint to be s
afe
			tint->interpolationValue = 0; // TODO: Check if this actually remo
ves the tint from being displayed?
			tint->tintIndex = 65535;
			tint->tintColor = RE::Color(255, 255, 255, 255);
			continue;
		}
		auto new_tint = raceutils::random_pick(matchedTints, rand_gen.GetNex
t());
		// Replace values of original tint with new tint, keeping closest co
lor match that’s within the asset’s presets
		tint->tintIndex = new_tint->texture.index;
		auto presetIdx = GetClosestPresetIdx(tint->tintColor, new_tint->pres
ets);
		tint->preset = presetIdx;
		auto alpha = tint->tintColor.alpha;
		tint->tintColor = new_tint->presets.colors[presetIdx]->color;
		tint->tintColor.alpha = alpha;
		// Update body tint color if this is for skin
		if (new_tint->texture.skinTone == RE::TESRace::FaceRelatedData::Tint
Asset::TintLayer::SkinTone::kSkinTone) {
			// This seems to make the skin tint correctly match the body tint
			tint->interpolationValue = 100;
			tint->tintColor.alpha = 255;
			a_data->bodyTintColor = tint->tintColor;
			bodyColorFixed = true;
			logger::info(" Skin tone changed to RGB:{}|{}|{}|{}", tint->tintCo
lor.red, tint->tintColor.green, tint->tintColor.blue, tint->tintColor.
alpha);
		}
	}
	//If npc doesn’t have skin tint layer, assign closest skin tint layer
	if (!bodyColorFixed && skintone_tintasset) {
		RE::TESNPC::Layer* skin_tint = utils::AllocateMemoryCleanly<RE::TESN
PC::Layer>();
		skin_tint->tintIndex = skintone_tintasset->texture.index;
		auto presetIdx = GetClosestPresetIdx(a_data->bodyTintColor, skintone
_tintasset->presets);
		skin_tint->preset = presetIdx;
		skin_tint->tintColor = skintone_tintasset->presets.colors[presetIdx]
->color;
		a_data->bodyTintColor = skin_tint->tintColor;
		// This seems to make the skin tint correctly match the body tint
		skin_tint->interpolationValue = 100;
		skin_tint->tintColor.alpha = 255;
		a_data->tintLayers->push_back(skin_tint);
		logger::info(" NPC has no skin tone. Skin tone assigned to RGB:{}|{
}|{}", skin_tint->tintColor.red, skin_tint->tintColor.green, skin_tint
->tintColor.blue);
	} else if (!skintone_tintasset) {
		logger::info(" NPC has no skin tone. And Race: {} Sex: {} has no de
fault skin tone.", utils::GetFormEditorID(a_data->race).c_str(), a_dat
a->isFemale);
		return false;
	}
	return true;
}
RE::BGSHeadPart* RaceSwap::SwitchHeadPart(raceutils::RandomGen rand_ge
n, NPCAppearance::NPCData* a_data, RE::BGSHeadPart* a_part)
{
	if (a_part == nullptr) {
		return a_part;
	}
	auto head_part_type = a_part->type.get();
	auto database = raceswap::DataBase::GetSingleton();
	auto& hdptd = *(database->FindOrCalculateHDPTData(a_part));
	auto item_list = database->GetMatchedHeadPartResults(head_part_type, 
static_cast<RE::SEX>(a_data->isFemale), a_data->race, hdptd);
	auto new_item = raceutils::random_pick(item_list, rand_gen.GetNext())
;
	if (new_item && !database->IsValidHeadPart(new_item)) {
		// TODO: Exclude invalid head parts?
		logger::warn(" {:x} is an invalid head part!", new_item->formID);
	}
	if (new_item) {
		return new_item;
	} else {
		logger::debug("New headpart is null, keeping original!");
		return a_part;
	}
}
//========================================End RaceSwap.cpp
Begin RaceSwap.h:
#pragma once
#include "PCH.h"
#include "RaceSwapUtils.h"
#include "NPCAppearance.h"
class RaceSwap
{
public:
	using HeadPartType = RE::BGSHeadPart::HeadPartType;
	static void applySwap(NPCAppearance::NPCData* a_data, RE::TESRace* a_
otherRace);
private:
	static bool DoHeadData(raceutils::RandomGen rand_gen, NPCAppearance::
NPCData* a_data);
	static bool DoHeadParts(raceutils::RandomGen rand_gen, NPCAppearance:
:NPCData* a_data);
	static bool DoHeadMorphs(raceutils::RandomGen rand_gen, NPCAppearance
::NPCData* a_data);
	static bool DoTints(raceutils::RandomGen rand_gen, NPCAppearance::NPC
Data* a_data, RE::TESRace* a_originalRace);
	static RE::BGSHeadPart* SwitchHeadPart(raceutils::RandomGen rand_gen,
 NPCAppearance::NPCData* a_data, RE::BGSHeadPart* a_part);
	static std::unordered_map<RE::BGSHeadPart*, raceutils::HDPTData*> _hd
ptd_cache;
};
//========================================End RaceSwap.h
Begin RaceSwapDatabase.h:
#pragma once
#include <string>
#include <algorithm>
#include <vector>
#include <functional>
#include <unordered_map>
#include <map>
#include "RaceSwapUtils.h"
namespace raceswap
{
	class DataBase
	{
	public:
		enum HDPTType : std::uint32_t
		{
			Mouth = 1 << 0,
			Beard = 1 << 1,
			Hair = 1 << 2,
			HairLine = 1 << 3,
			Eyes = 1 << 4,
			Scar = 1 << 5,
			Brow = 1 << 6,
			Earring = 1 << 7,
			Mustache = 1 << 8,
			Ear = 1 << 9,
			Mark = 1 << 10,
			Horn = 1 << 11,
		};
		static inline std::map<std::string, HDPTType> HDPTTypeMap{ 
			std::pair("Mouth", Mouth),
			std::pair("Beard", Beard),
			std::pair("Hair", Hair),
			std::pair("HairLine", HairLine),
			std::pair("Hairline", HairLine),
			std::pair("Eyes", Eyes),
			std::pair("Scar", Scar),
			std::pair("Brow", Brow),
			std::pair("Earring", Earring),
			std::pair("Mustache", Mustache),
			std::pair("Ear", Ear),
			std::pair("Mark", Mark),
			std::pair("Horn", Horn),
		};
		enum HDPTCharacteristics : std::uint32_t
		{
			Blind = 1 << 0,
			Left = 1 << 1,
			Right = 1 << 2,
			NoHair = 1 << 4,
			Demon = 1 << 5,
			Vampire = 1 << 6,
			Shaved = 1 << 7,
			NoScar = 1 << 8,
			Gash = 1 << 9,
			NoGash = 1 << 10,
			Small = 1 << 11,
			Medium = 1 << 12,
			Long = 1 << 13,
			NoBeard = 1 << 14,
			Narrow = 1 << 15,
		};
		static inline std::map<std::string, HDPTCharacteristics> HDPTCharMap
{
			std::pair("Blind", Blind),
			std::pair("Left", Left),
			std::pair("Right", Right),
			std::pair("NoHair", NoHair),
			std::pair("Nohair", NoHair),
			std::pair("Demon", Demon),
			std::pair("Vampire", Vampire),
			std::pair("Shaved", Shaved),
			std::pair("NoScar", NoScar),
			std::pair("Noscar", NoScar),
			std::pair("Gash", Gash),
			std::pair("NoGash", NoGash),
			std::pair("Nogash", NoGash),
			std::pair("Small", Small),
			std::pair("Medium", Medium),
			std::pair("Long", Long),
			std::pair("NoBeard", NoBeard),
			std::pair("Nobeard", NoBeard),
			std::pair("Narrow", Narrow),
		};
		enum HDPTColor : std::uint32_t
		{
			Hazel = 1 << 0,
			Brown = 1 << 1,
			Yellow = 1 << 2,
			Blue = 1 << 3,
			Light = 1 << 4,
			Dark = 1 << 5,
			Ice = 1 << 6,
			Orange = 1 << 7,
			Green = 1 << 8,
			Grey = 1 << 9,
			Red = 1 << 10,
			Deep = 1 << 11,
			Violet = 1 << 12,
			Bright = 1 << 13,
			Aqua = 1 << 14,
			Silver = 1 << 15,
			Olive = 1 << 16,
		};
		static inline std::map<std::string, HDPTColor> HDPTColorMap{
			std::pair("Hazel", Hazel),
			std::pair("Brown", Brown),
			std::pair("Yellow", Yellow),
			std::pair("Blue", Blue),
			std::pair("Light", Light),
			std::pair("Dark", Dark),
			std::pair("Ice", Ice),
			std::pair("Orange", Orange),
			std::pair("Green", Green),
			std::pair("Grey", Grey),
			std::pair("Red", Red),
			std::pair("Deep", Deep),
			std::pair("Violet", Violet),
			std::pair("Bright", Bright),
			std::pair("Aqua", Aqua),
			std::pair("Silver", Silver),
			std::pair("Olive", Olive),
		};
		enum SkinTextureCharacteristics : std::uint32_t
		{
			Complexion = 1 << 0,
			Rough = 1 << 1,
			Freckles = 1 << 2,
			Age = 1 << 4,
		};
		static inline std::map<std::string, SkinTextureCharacteristics> Skin
CharMap{
			std::pair("Complexion", Complexion),
			std::pair("Rough", Rough),
			std::pair("Freckles", Freckles),
			std::pair("Frekles", Freckles),
			std::pair("Age", Age),
		};
		// HeadPartType , Characteristics, Color
		using HDPTData = std::tuple<std::uint32_t, std::uint32_t, std::uint3
2_t>;
		using SkinTextureData = std::uint32_t;
		using _likelihood_t = uint8_t;
		using HeadPartType = RE::BGSHeadPart::HeadPartType;
		using TintType = RE::TESRace::FaceRelatedData::TintAsset::TintLayer:
:SkinTone;
		std::vector<RE::TESRace*> valid_races;
		std::unordered_map<RE::SEX, std::unordered_map<HeadPartType, std::un
ordered_map<RE::TESRace*, std::vector<RE::BGSHeadPart*>>>> valid_type_
race_headpart_map;
		std::unordered_map<RE::SEX, std::unordered_map<HeadPartType, std::un
ordered_map<RE::TESRace*, std::vector<HDPTData*>>>> valid_type_race_HD
PTdata_map;
		std::unordered_map<RE::SEX, std::unordered_map<RE::TESRace*, RE::TES
Race::FaceRelatedData::TintAsset*>> default_skintint_for_each_race;
		std::unordered_set<RE::BGSHeadPart*> usedHeadparts;
		bool dumplists;
		inline static DataBase* & GetSingleton(bool _dumplists = true)
		{
			static DataBase* _this_database = nullptr;
			if (!_this_database)
				_this_database = new DataBase(_dumplists);
			return _this_database;
		}
		HDPTData* FindOrCalculateHDPTData(RE::BGSHeadPart* hdpt)
		{
			if (hdpt == nullptr) {
				logger::warn("Null headpart being parsed as HDPT");
				return nullptr;
			}
			auto iter = _hdptd_cache.find(hdpt);
			if (iter == _hdptd_cache.end()) {
				HDPTData* hdptd_ptr = new HDPTData(raceutils::ExtractKeywords(hdpt
));
				_hdptd_cache[hdpt] = hdptd_ptr;
				return hdptd_ptr;
			} else {
				return iter->second;
			}
		}
		inline std::vector<RE::BGSHeadPart*> GetHeadParts(HeadPartType type,
 RE::SEX sex, RE::TESRace* race)
		{
			if (utils::is_amongst(valid_type_race_headpart_map, sex)) {
				if (utils::is_amongst(valid_type_race_headpart_map[sex], type)) {
					if (utils::is_amongst(valid_type_race_headpart_map[sex][type], ra
ce)) {
						return valid_type_race_headpart_map[sex][type][race];
					}
				}
			}
			return std::vector<RE::BGSHeadPart*>();
		}
		inline std::vector<HDPTData*> GetHDPTData(HeadPartType type, RE::SEX
 sex, RE::TESRace* race)
		{
			if (utils::is_amongst(valid_type_race_HDPTdata_map, sex)) {
				if (utils::is_amongst(valid_type_race_HDPTdata_map[sex], type)) {
					if (utils::is_amongst(valid_type_race_HDPTdata_map[sex][type], ra
ce)) {
						return valid_type_race_HDPTdata_map[sex][type][race];
					}
				}
			}
			return std::vector<HDPTData*>();
		}
		inline std::vector<RE::BGSHeadPart*> GetMatchedHeadPartResults(HeadP
artType type, RE::SEX sex, RE::TESRace* race, RE::BGSHeadPart* hdpt) {
			return GetMatchedHeadPartResults(type, sex, race, *FindOrCalculateH
DPTData(hdpt));
		}
		inline std::vector<RE::BGSHeadPart*> GetMatchedHeadPartResults(HeadP
artType type, RE::SEX sex, RE::TESRace* race, HDPTData hdpt)
		{
			auto hdpts = GetHeadParts(type, sex, race);
			auto hdptd = GetHDPTData(type, sex, race);
			return raceutils::MatchHDPTData(hdpt, hdpts, hdptd);
		}
		inline std::vector<RE::BGSTextureSet*> GetMatchedSkinTextureResults(
RE::SEX sex, RE::TESRace* race, RE::BGSTextureSet* skindata)
		{
			std::vector<RE::BGSTextureSet*> skinTextures;
			std::vector<SkinTextureData> skinTexturesChars;
			for (auto skinTexture : *race->faceRelatedData[sex]->faceDetailsTex
tureSets) {
				skinTextures.push_back(skinTexture);
				skinTexturesChars.push_back(raceutils::ExtractKeywords(skinTexture
));
			}
			auto dstTextureData = raceutils::ExtractKeywords(skindata);
			return raceutils::MatchSkinTextureData(dstTextureData, skinTextures
, skinTexturesChars);
		}
		RE::TESRace::FaceRelatedData::TintAsset* GetRaceSkinTint(RE::SEX sex
, RE::TESRace* race) {
			return default_skintint_for_each_race[sex][race];
		}
		RE::TESRace* GetDefaultRace() {
			return RE::TESForm::LookupByID(0x19)->As<RE::TESRace>();
		}
		bool IsValidHeadPart(RE::BGSHeadPart* hdpt)
		{
			if (hdpt->extraParts.empty() && hdpt->model.empty() && utils::GetFo
rmEditorID(hdpt).find("No") == std::string::npos) {
				return false;
			}
			if (hdpt->validRaces == nullptr) {
				return false;
			}
			if (!hdpt->flags.any(RE::BGSHeadPart::Flag::kPlayable) &&
				!usedHeadparts.contains(hdpt)) {
				// Headpart is not used, and the player cannot use the headpart, t
reat as unwanted
				// Prevents exploding heads in the case of Skyfurry, which has bad
 unused headparts
				return false;
			}
			auto modelFilePath = "meshes\\" + std::string(hdpt->model.c_str());
			if (!RE::BSResourceNiBinaryStream(modelFilePath).good() && !hdpt->m
odel.empty()) {
				// model filename does not exist in BSA archive or as loose file
				logger::debug("{} is not a valid resource for {:x}!", modelFilePat
h, hdpt->formID);
				return false;
			}
			return true;
		}
		/*
		@brief Safely de-allocate the memory space used by DataBase.
		*/
		static void Dealloc() {
			delete GetSingleton();
			GetSingleton() = nullptr;
		}
	private:
		DataBase(bool _dumplists): dumplists(_dumplists)
		{
			_initialize();
		}
		void _initialize() {
			auto const& [map, lock] = RE::TESForm::GetAllForms();
			lock.get().LockForRead();
			logger::info("Begin categorizing...");
			// Populate NPC used headparsts
			for (auto const& [formid, form] : *map) {
				//To do
				if ((formid & 0xFF000000) == 0xFF000000)
					continue;
				if (form->Is(RE::FormType::NPC)) {
					auto& headparts = form->As<RE::TESNPC>()->headParts;
					auto numHeadParts = form->As<RE::TESNPC>()->numHeadParts;
					for (std::uint8_t i = 0; i < numHeadParts; i++) {
						auto& headpart = headparts[i];
						usedHeadparts.emplace(headpart);
						for (auto extra : headpart->extraParts) {
							usedHeadparts.emplace(extra);
						}
					}
				}
			}
			//Categorizing head parts and calculate their discriptors (HDPTData
) for matching
			for (auto const& [formid, form] : *map) {
				//To do
				if ((formid & 0xFF000000) == 0xFF000000)
					continue;
				if (form->Is(RE::FormType::HeadPart)) {
					auto hdpt = form->As<RE::BGSHeadPart>();
					auto _append_to_list_male = [hdpt, this](RE::TESForm& form) { 
						valid_type_race_headpart_map[RE::SEX::kMale][hdpt->type.get()][f
orm.As<RE::TESRace>()].push_back(hdpt);
						valid_type_race_HDPTdata_map[RE::SEX::kMale][hdpt->type.get()][f
orm.As<RE::TESRace>()].push_back(FindOrCalculateHDPTData(hdpt));
						return RE::BSContainer::ForEachResult::kContinue;
					};
					auto _append_to_list_female = [hdpt, this](RE::TESForm& form) {
						valid_type_race_headpart_map[RE::SEX::kFemale][hdpt->type.get()]
[form.As<RE::TESRace>()].push_back(hdpt);
						valid_type_race_HDPTdata_map[RE::SEX::kFemale][hdpt->type.get()]
[form.As<RE::TESRace>()].push_back(FindOrCalculateHDPTData(hdpt));
						return RE::BSContainer::ForEachResult::kContinue;
					};
					if (IsValidHeadPart(hdpt)) {
						if (hdpt->flags.any(RE::BGSHeadPart::Flag::kMale)) {
							hdpt->validRaces->ForEachForm(_append_to_list_male);
						}
						if (hdpt->flags.any(RE::BGSHeadPart::Flag::kFemale)) {
							hdpt->validRaces->ForEachForm(_append_to_list_female);
						}
					}
				} else if (form->Is(RE::FormType::Race)) {
					if (form->As<RE::TESRace>()->faceRelatedData[RE::SEX::kMale] != n
ullptr && 
						form->As<RE::TESRace>()->faceRelatedData[RE::SEX::kMale]->tintMa
sks != nullptr) {
						// TODO: Could we support animal races with no face related data
?
						// TODO: We could make this cleaner/simpler
						valid_races.push_back(form->As<RE::TESRace>());
					}
				}
			}
			//Collecting other assets
			for (auto race : valid_races) {
				auto race_male_tints = race->faceRelatedData[RE::SEX::kMale]->tint
Masks;
				auto race_female_tints = race->faceRelatedData[RE::SEX::kFemale]->
tintMasks;
				for (auto race_tint : *race_male_tints) {
					if (race_tint->texture.skinTone == DataBase::TintType::kSkinTone)
 {
						this->default_skintint_for_each_race[RE::SEX::kMale][race] = rac
e_tint;
						break;
					}
				}
				if (!this->default_skintint_for_each_race[RE::SEX::kMale][race]) {
					logger::info(" Race: {} has no skin tone tint layer for male.", 
utils::GetFormEditorID(race));
				}
				for (auto race_tint : *race_female_tints) {
					if (race_tint->texture.skinTone == DataBase::TintType::kSkinTone)
 {
						this->default_skintint_for_each_race[RE::SEX::kFemale][race] = r
ace_tint;
						break;
					}
				}
				if (!this->default_skintint_for_each_race[RE::SEX::kFemale][race])
 {
					logger::info(" Race: {} has no skin tone tint layer for female."
, utils::GetFormEditorID(race));
				}
			}
			lock.get().UnlockForRead();
			logger::info("Finished categorizing...");
		}
		std::unordered_map<RE::BGSHeadPart*, HDPTData*> _hdptd_cache;
	};
}
//========================================End RaceSwapDatabase.h
Begin RaceSwapUtils.cpp:
#include "RaceSwapUtils.h"
#include "PCH.h"
#include "RaceSwapDatabase.h"
namespace raceutils
{
	HDPTData ExtractKeywords(RE::BGSHeadPart* hdpt)
	{
		std::uint32_t types = 0;
		std::uint32_t characteristics = 0;
		std::uint32_t colors = 0;
		std::string str(utils::GetFormEditorID(hdpt));
		if (str == "") {
			logger::info("{:x} has no editor ID!");
		}
		for (auto& [typeString, type] : raceswap::DataBase::HDPTTypeMap) {
			if (str.find(typeString) != std::string::npos) {
				types |= type;
			}
		}
		for (auto& [charString, characteristic] : raceswap::DataBase::HDPTCh
arMap) {
			if (str.find(charString) != std::string::npos) {
				characteristics |= characteristic;
			}
		}
		for (auto& [colorString, color] : raceswap::DataBase::HDPTColorMap) 
{
			if (str.find(colorString) != std::string::npos) {
				colors |= color;
			}
		}
		HDPTData result = { types, characteristics, colors };
		return result;
	}
	_likelihood_t _match(HDPTData dst, HDPTData src)
	{
		_likelihood_t likelihood = 0;
		auto typesMatch = std::bitset<32>(std::get<0>(dst) & std::get<0>(src
)).count();
		likelihood += (_likelihood_t) typesMatch;
		likelihood = likelihood << 4;
		auto charsMatch = std::bitset<32>(std::get<1>(dst) & std::get<1>(src
)).count();
		likelihood += (_likelihood_t) charsMatch;
		likelihood = likelihood << 4;
		auto colorsMatch = std::bitset<32>(std::get<2>(dst) & std::get<2>(sr
c)).count();
		likelihood += (_likelihood_t) colorsMatch;
		return likelihood;
	}
	std::vector<RE::BGSHeadPart*> MatchHDPTData(HDPTData dst, std::vector
<RE::BGSHeadPart*> src_hdpts, std::vector<HDPTData*> src_data)
	{
		if (src_hdpts.size() != src_data.size()) {
			return std::vector<RE::BGSHeadPart*>();
		}
		std::map<_likelihood_t, std::vector<RE::BGSHeadPart*>> likelihood_ma
p;
		_likelihood_t max = 0;
		for (int i = 0; i < src_data.size(); i++) {
			auto likelihood = _match(dst, *(src_data[i]));
			if (likelihood > max)
				max = likelihood;
			likelihood_map[likelihood].push_back(src_hdpts[i]);
		}
		return likelihood_map[max];
	}
	SkinTextureData ExtractKeywords(RE::BGSTextureSet* hdpt)
	{
		std::uint32_t characteristics = 0;
		std::string str(utils::GetFormEditorID(hdpt));
		if (str == "") {
			logger::info("{:x} has no editor ID!");
		}
		for (auto& [charString, characteristic] : raceswap::DataBase::SkinCh
arMap) {
			if (str.find(charString) != std::string::npos) {
				characteristics |= characteristic;
			}
		}
		SkinTextureData result = { characteristics };
		return result;
	}
	_likelihood_t _match(SkinTextureData dst, SkinTextureData src)
	{
		_likelihood_t likelihood = 0;
		auto charsMatch = std::bitset<32>(dst & src).count();
		likelihood += (_likelihood_t)charsMatch;
		likelihood = likelihood << 4;
		return likelihood;
	}
	std::vector<RE::BGSTextureSet*> MatchSkinTextureData(SkinTextureData 
dst, std::vector<RE::BGSTextureSet*> src_hdpts, std::vector<SkinTextur
eData> src_data)
	{
		if (src_hdpts.size() != src_data.size()) {
			return std::vector<RE::BGSTextureSet*>();
		}
		std::map<_likelihood_t, std::vector<RE::BGSTextureSet*>> likelihood_
map;
		_likelihood_t max = 0;
		for (int i = 0; i < src_data.size(); i++) {
			auto likelihood = _match(dst, src_data[i]);
			if (likelihood > max)
				max = likelihood;
			likelihood_map[likelihood].push_back(src_hdpts[i]);
		}
		return likelihood_map[max];
	}
}
//========================================End RaceSwapUtils.cpp
Begin RaceSwapUtils.h:
#pragma once
#include "PCH.h"
#include <random>
#include <functional>
#include <string>
#include <unordered_map>
#include "Utils.h"
namespace raceutils
{
	template <class T>
	T random_pick(std::vector<T> item_list, size_t rand)
	{
		if (item_list.empty())
			return 0;
		return item_list[rand % item_list.size()];
	}
	template <class T>
	T random_pick(RE::BSTArray<T> item_list, size_t rand)
	{
		if (item_list.empty())
			return 0;
		return item_list[rand % item_list.size()];
	}
	using HDPTData = std::tuple<std::uint32_t, std::uint32_t, std::uint32
_t>;
	using SkinTextureData = std::uint32_t;
	using _likelihood_t = uint16_t;
	HDPTData ExtractKeywords(RE::BGSHeadPart* hdpt);
	_likelihood_t _match(HDPTData dst, HDPTData src);
	std::vector<RE::BGSHeadPart*> MatchHDPTData(HDPTData dst, std::vector
<RE::BGSHeadPart*> src_hdpts, std::vector<HDPTData*> src_data);
	SkinTextureData ExtractKeywords(RE::BGSTextureSet* hdpt);
	_likelihood_t _match(SkinTextureData dst, SkinTextureData src);
	std::vector<RE::BGSTextureSet*> MatchSkinTextureData(SkinTextureData 
dst, std::vector<RE::BGSTextureSet*> src_hdpts, std::vector<SkinTextur
eData> src_data);
	/* 
	Class for random number generation based on a TESForm.
	Example:
		util::RandomGen<RE::TESForm> generator(some_form_ptr, util::UniqueSt
ringFromForm);
		auto hash = generator(0)
		auto random1 = generator.GetNext();
		auto random2 = generator.GetStableRandom(1);
		return random1 == random2 //true
	*/
	class RandomGen
	{
	public:
		RandomGen(RE::TESForm* a_item_seed) :
			form_seed(a_item_seed)
		{
			_hash_seed = utils::HashForm(form_seed);
			_random_num = _hash_seed;
		}
		inline const size_t GetHashSeed() const {
			return _hash_seed;
		}
		//@brief Get the Nth random number generated from the hash seed.
		size_t GetStableRandom(std::uint32_t n_th_random = 1){
			_random_num = _hash_seed;
			for (std::uint32_t i = 0; i < n_th_random; i++) {
				GetNext();
			}
			return _random_num;
		}
		//@brief Get the next random number generated from the previous rand
om number.
		size_t GetNext(){
			srand((int) _random_num);
			_random_num = rand();
			srand(clock());
			return _random_num;
		}
		//@brief Get the Nth random number generated from the hash seed.
		size_t operator()(unsigned int n_th_random) {
			return GetStableRandom(n_th_random);
		}
	private:
		RandomGen();
		RE::TESForm* form_seed;
		size_t _hash_seed;
		size_t _random_num;
	};
}
//========================================End RaceSwapUtils.h
Begin Settings.cpp:
#include "Settings.h"
Settings* Settings::GetSingleton()
{
	static Settings singleton;
	return std::addressof(singleton);
}
void Settings::Load()
{
	constexpr auto path = L"Data/SKSE/Plugins/RaceSwapper.ini";
	// Vˆ'rifiez si le rˆ'pertoire existe
	std::filesystem::path dirPath = L"Data/SKSE/Plugins/";
	if (!std::filesystem::exists(dirPath)) {
		// Crˆ'ez le rˆ'pertoire s’il n’existe pas
		std::filesystem::create_directories(dirPath);
	}
	CSimpleIniA ini;
	ini.SetUnicode();
	ini.LoadFile(path);
	const char* section = "Features";
	get_value(ini, Features::kPlaythroughRandomization, false, section, "
bRandomizePerPlaythrough", "; In each playthough, RaceSwapper will alt
er NPCs differently, giving a different appearance");
	ini.SaveFile(path);
}
//========================================End Settings.cpp
Begin Settings.h:
#pragma once
class Settings
{
public:
	[[nodiscard]] static Settings* GetSingleton();
	void Load();
	enum Features : std::uint32_t
	{
		kNone = 0,
		kPlaythroughRandomization = 1 << 0,
	};
	stl::enumeration<Features, std::uint32_t> features;
private:
	void get_value(CSimpleIniA& a_ini, Features a_value, bool a_default, 
const char* a_section, const char* a_key, const char* a_comment)
	{
		auto value = a_ini.GetBoolValue(a_section, a_key, a_default);
		a_ini.SetBoolValue(a_section, a_key, value, a_comment);
		if (value) {
			features.set(a_value);
		} else {
			features.reset(a_value);
		}
	}
};
//========================================End Settings.h
Begin Utils.cpp:
#include "Utils.h"
#include "PCH.h"
#include "settings/Settings.h"
namespace utils
{
	std::string UniqueStringFromForm(RE::TESForm* a_form_seed)
	{
		if (!a_form_seed) {
			return std::string();
		}
		auto rawFormID = std::to_string(a_form_seed->GetFormID() & 0x00FFFFF
F);
		auto fileName = "DynamicForm";
		if (!a_form_seed->IsDynamicForm()) {
			fileName = a_form_seed->GetFile()->fileName;
		}
		std::string playthroughID = ""; 
		if (Settings::GetSingleton()->features.any(Settings::Features::kPlay
throughRandomization)) {
			playthroughID = std::to_string(RE::BGSSaveLoadManager::GetSingleton
()->currentPlayerID);
		}
		return rawFormID + "_" + fileName + "_" + playthroughID;
	}
	size_t HashForm(RE::TESForm* a_form_seed)
	{
		std::string data = UniqueStringFromForm(a_form_seed);
		long p = 16777619;
		size_t hash = 2166136261L;
		for (int i = 0; i < data.length(); i++) {
			hash = (hash ^ data[i]) * p;
			hash += hash << 13;
			hash ^= hash >> 7;
			hash += hash << 3;
			hash ^= hash << 17;
			hash += hash >> 5;
		}
		// TODO: Use currentPlayerID to have different hash/"seed" for each 
playthrough
		//if (true) {
		//	hash += RE::BGSSaveLoadManager::GetSingleton()->currentPlayerID;
		//} 
		return hash;
	}
	RE::BSTArray<RE::TESNPC::Layer*>* CopyTintLayers(RE::BSTArray<RE::TES
NPC::Layer*>* a_tintLayers)
	{
		if (!a_tintLayers) {
			return nullptr;
		}
		auto copiedTintLayers = utils::AllocateMemoryCleanly<RE::BSTArray<RE
::TESNPC::Layer*>>();
		if (!a_tintLayers->empty()) {
			for (auto tint : *a_tintLayers) {
				auto newLayer = AllocateMemoryCleanly<RE::TESNPC::Layer>();
				newLayer->tintColor = tint->tintColor;
				newLayer->tintIndex = tint->tintIndex;
				newLayer->preset = tint->preset;
				newLayer->interpolationValue = tint->interpolationValue;
				copiedTintLayers->emplace_back(newLayer);
			}
		}
		return copiedTintLayers;
	}
	RE::TESNPC::HeadRelatedData* CopyHeadRelatedData(RE::TESNPC::HeadRela
tedData* a_data)
	{
		auto newHeadData = AllocateMemoryCleanly<RE::TESNPC::HeadRelatedData
>();
		if (a_data) {
			newHeadData->hairColor = a_data->hairColor;
			newHeadData->faceDetails = a_data->faceDetails;
		}
		return newHeadData;
	}
	RE::BGSHeadPart** CopyHeadParts(RE::BGSHeadPart** a_parts, std::uint3
2_t a_numHeadParts)
	{
		if (!a_parts) {
			return nullptr;
		}
		auto newHeadParts = AllocateMemoryCleanly<RE::BGSHeadPart*>(sizeof(v
oid*) * a_numHeadParts);
		for (std::uint32_t index = 0; index < a_numHeadParts; index++) {
			newHeadParts[index] = a_parts[index];
		}
		return newHeadParts;
	}
	RE::TESNPC::FaceData* DeepCopyFaceData(RE::TESNPC::FaceData* a_faceDa
ta)
	{
		if (!a_faceData) {
			return nullptr;
		}
		auto newFaceData = AllocateMemoryCleanly<RE::TESNPC::FaceData>();
		for (std::uint32_t i = 0; i < 19; i++) {
			newFaceData->morphs[i] = a_faceData->morphs[i];
		}
		for (std::uint32_t i = 0; i < 4; i++) {
			newFaceData->parts[i] = a_faceData->parts[i];
		}
		return newFaceData;
	}
	std::vector<std::string> split_string(std::string& a_string, char a_d
elimiter)
	{
		std::vector<std::string> list;
		std::string strCopy = a_string;
		size_t pos = 0;
		std::string token;
		while ((pos = strCopy.find(a_delimiter)) != std::string::npos) {
			token = strCopy.substr(0, pos);
			list.push_back(token);
			strCopy.erase(0, pos + 1);
		}
		list.push_back(strCopy);
		return list;
	}
	std::string GetEditorID(RE::FormID a_formID)
	{
		static auto tweaks = GetModuleHandle(L"po3_Tweaks");
		static auto function = reinterpret_cast<_GetFormEditorID>(GetProcAdd
ress(tweaks, "GetFormEditorID"));
		if (function) {
			return function(a_formID);
		}
		return {};
	}
	std::string GetFormEditorID(const RE::TESForm* a_form)
	{
		if (!a_form) {
			return {};
		}
		if (a_form->IsDynamicForm()) {
			return a_form->GetFormEditorID();
		}
		switch (a_form->GetFormType()) {
		case RE::FormType::Keyword:
		case RE::FormType::LocationRefType:
		case RE::FormType::Action:
		case RE::FormType::MenuIcon:
		case RE::FormType::Global:
		case RE::FormType::HeadPart:
		case RE::FormType::Race:
		case RE::FormType::Sound:
		case RE::FormType::Script:
		case RE::FormType::Navigation:
		case RE::FormType::Cell:
		case RE::FormType::WorldSpace:
		case RE::FormType::Land:
		case RE::FormType::NavMesh:
		case RE::FormType::Dialogue:
		case RE::FormType::Quest:
		case RE::FormType::Idle:
		case RE::FormType::AnimatedObject:
		case RE::FormType::ImageAdapter:
		case RE::FormType::VoiceType:
		case RE::FormType::Ragdoll:
		case RE::FormType::DefaultObject:
		case RE::FormType::MusicType:
		case RE::FormType::StoryManagerBranchNode:
		case RE::FormType::StoryManagerQuestNode:
		case RE::FormType::StoryManagerEventNode:
		case RE::FormType::SoundRecord:
			return a_form->GetFormEditorID();
		default:
			return GetEditorID(a_form->GetFormID());
		}
	};
	RE::TESRace* GetValidRaceForArmorRecursive(RE::TESObjectARMO* a_armor
, RE::TESRace* a_race) {
		if (a_race == nullptr) {
			return nullptr;
		}
		bool isValidRace = false;
		for (auto addon : a_armor->armorAddons) {
			if (addon->race == a_race || is_amongst(addon->additionalRaces, a_r
ace)) {
				isValidRace = true;
				break;
			}
		}
		return isValidRace ? a_race : GetValidRaceForArmorRecursive(a_armor,
 a_race->armorParentRace);
	}
	template <class T>
	T* AllocateMemoryCleanly() {
		return AllocateMemoryCleanly<T>((std::uint32_t) sizeof(T));
	}
	template <class T>
	T* AllocateMemoryCleanly(std::uint32_t a_size)
	{
		auto data = RE::malloc(a_size);
		std::memset(data, 0, a_size);
		return reinterpret_cast<T*>(data);
	}
}
//========================================End Utils.cpp
Begin Utils.h:
#pragma once
namespace utils
{
	using _GetFormEditorID = const char* (*)(std::uint32_t);
	std::string UniqueStringFromForm(RE::TESForm* a_form_seed);
	size_t HashForm(RE::TESForm* a_form_seed);
	RE::BSTArray<RE::TESNPC::Layer*>* CopyTintLayers(RE::BSTArray<RE::TES
NPC::Layer*>* a_tintLayers);
	RE::TESNPC::HeadRelatedData* CopyHeadRelatedData(RE::TESNPC::HeadRela
tedData* a_data);
	RE::BGSHeadPart** CopyHeadParts(RE::BGSHeadPart** a_parts, std::uint3
2_t a_numHeadParts);
	RE::TESNPC::FaceData* DeepCopyFaceData(RE::TESNPC::FaceData* a_faceDa
ta);
	std::vector<std::string> split_string(std::string& a_string, char a_d
elimiter);
	std::string GetFormEditorID(const RE::TESForm* a_form);
	template <class T>
	bool is_amongst(std::vector<T> item_list, T elem)
	{
		return std::find(item_list.begin(), item_list.end(), elem) != item_l
ist.end();
	}
	template <class T>
	bool is_amongst(RE::BSTArray<T> item_list, T elem)
	{
		return std::find(item_list.begin(), item_list.end(), elem) != item_l
ist.end();
	}
	template <class _First_T, class _Second_T>
	bool is_amongst(std::unordered_map<_First_T, _Second_T> item_map, _Fi
rst_T elem)
	{
		return item_map.find(elem) != item_map.end();
	}
	RE::TESRace* GetValidRaceForArmorRecursive(RE::TESObjectARMO* a_armor
, RE::TESRace* a_race);
	// Allocates memory for T in a clean way (AKA no garbage data in the 
new structure)
	template <class T>
	T* AllocateMemoryCleanly();
	// Allocates memory for T in a clean way (AKA no garbage data in the 
new structure) with a specific size
	template <class T>
	T* AllocateMemoryCleanly(std::uint32_t a_size);
}
//========================================End Utils.h
