use crate::vm::def::StackLoc;
use crate::vm::{InstructionAddress, CodeChunk, StackSlot, StackValue, UpValue};
use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;

pub type MemoryAddress = usize;


/// Simple abstraction of a "heap" like object, so we can easily refer to things.
/// In practice, the implementation may change. For now it's just a vec.
#[derive(Debug)]
pub struct Heap {
    values: Vec<StackValue>,
}
impl  Heap {

    pub fn new() -> Self {
        Self {
            values: Vec::with_capacity(1024)
        }
    }

    pub fn alloc(&mut self, value: StackValue) -> MemoryAddress {
        let ret = self.values.len();
        self.values.push(value);
        ret
    }

    pub fn read(&self, index: MemoryAddress) -> &StackValue {
        &self.values[index]
    }

    pub fn read_mut(&mut self, index: MemoryAddress) -> &mut StackValue {
        &mut self.values[index]
    }

    pub fn write(&mut self, index: MemoryAddress, value: StackValue) {
        self.values[index] = value;
    }
}


#[derive(Debug, Clone)]
pub struct UpValueStorage {
    upvalues: Vec<UpValue>,
    open_upvalues: HashMap<StackLoc, usize>
}
impl UpValueStorage {
    pub fn new() -> Self {
        Self {
            upvalues :Vec::new(),
            open_upvalues: HashMap::new()
        }
    }

    pub fn push_open_upvalue(&mut self, loc: StackLoc) -> usize {
        let idx = self.upvalues.len();
        let value = UpValue::Open(loc);
        self.upvalues.push(value);
        self.open_upvalues.insert(loc, idx);
        idx
    }

    pub fn get_open_upvalue_idx(&self, loc: &StackLoc) -> Option<usize> {
        self.open_upvalues.get(loc).cloned()
    }

    pub fn get(&self, index: usize) -> &UpValue {
        &self.upvalues[index]
    }

    pub fn get_mut(&mut self, index: usize) -> &mut UpValue {
        &mut self.upvalues[index]
    }

    pub fn close_upvalue(&mut self, index: usize, value: MemoryAddress) {
        match &self.upvalues[index] {
            UpValue::Open(loc) => {
                self.open_upvalues.remove(loc);
            }
            UpValue::Closed(_) => {
                panic!("Cannot close an already closed upvalue!")
            }
        }
        self.upvalues[index] = UpValue::Closed(value);
    }
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    base_index: usize,
    slots: usize,
    pub return_addr: InstructionAddress,
    // vector of upvalue indices
    upvalues: Rc<[usize]>,
    code_chunk: usize,
}
impl CallFrame {
    fn base() -> Self {
        Self {
            base_index: 0,
            slots: 0,
            return_addr: 0,
            upvalues: Rc::new([]),
            code_chunk: 0,
        }
    }

    pub fn upvalue_idx(&self, index: usize) -> usize {
        self.upvalues[index]
    }
}


const MAX_STACK_SIZE: usize = 1024 * 1024 * 8 / 32;
const MAX_FRAMES: usize = 1024;

pub struct CallStack {
    data: Box<[Option<StackSlot>]>,
    call_frames: Box<[Option<CallFrame>]>,

    // index that points to the current stack frame
    frame_pointer: usize,
}
impl CallStack {
    pub fn new() -> Self {
        let mut call_frames = vec![None; MAX_FRAMES].into_boxed_slice();
        call_frames[0] = Some(CallFrame::base());
        Self {
            call_frames,
            data: vec![None; MAX_STACK_SIZE].into_boxed_slice(),
            frame_pointer: 0,
        }
    }


    /// Pushes a new Stack frame
    pub fn push_frame(&mut self, return_addr: InstructionAddress, upvalues: Rc<[usize]>, code_chunk: usize) {
        let last = self.call_frames[self.frame_pointer].as_ref().unwrap();
        let base_index = last.base_index + last.slots;
        self.frame_pointer += 1;
        self.call_frames[self.frame_pointer] = Some(CallFrame {
            base_index,
            return_addr,
            upvalues,
            code_chunk,
            slots: 0,
        });
    }

    /// Pops the top stack frame
    pub fn pop_frame(&mut self) -> CallFrame {
        // cannot pop the base frame ever
        debug_assert!(self.frame_pointer > 0);
        let old = std::mem::replace(&mut self.call_frames[self.frame_pointer], None).unwrap();
        self.frame_pointer -= 1;
        old
    }

    pub fn store(&mut self, index: usize, value: StackValue) {
        let top = self._top();
        let (real_index, slots) = (top.base_index + index, top.slots);
        self.data[real_index] = Some(StackSlot {
            captured: None,
            data: value,
        });
        if index >= slots {
            self._top_mut().slots = index + 1;
        }
    }
    pub fn load(&self, index: usize) -> &StackValue {
        let index = self._top().base_index + index;
        &self.data[index].as_ref().unwrap().data
    }
    pub fn load_mut(&mut self, index: usize) -> &mut StackValue {
        let index = self._top().base_index + index;
        &mut self.data[index].as_mut().unwrap().data
    }

    pub fn get_slot(&self, loc: &StackLoc) -> &StackSlot {
        let base = self.call_frames[loc.frame].as_ref().unwrap().base_index;
        self.data[base + loc.slot].as_ref().unwrap()
    }

    pub fn get_slot_mut(&mut self, loc: &StackLoc) -> &mut StackSlot {
        let base = self.call_frames[loc.frame].as_ref().unwrap().base_index;
        self.data[base + loc.slot].as_mut().unwrap()
    }

    pub fn current_code(&self) -> usize {
        self._top().code_chunk
    }
    pub fn is_main(&self) -> bool {
        self.frame_pointer == 0
    }

    pub fn upvalue_index(&self, index: usize) -> usize {
        self._top().upvalues[index]
    }

    pub fn frame_len(&self) -> usize {
        self.frame_pointer
    }


    pub fn slot_iter(&self) -> core::slice::Iter<'_, Option<StackSlot>> {
        self.data[self._top_slots()].iter()
    }

    pub fn slot_iter_mut(&mut self) -> core::slice::IterMut<'_, Option<StackSlot>> {
        let range = self._top_slots();
        self.data[range].iter_mut()
    }

    fn _top_slots(&self) -> Range<usize> {
        let top = self.call_frames[self.frame_pointer].as_ref().unwrap();
        top.base_index..top.base_index + top.slots
    }

    fn _top(&self) -> &CallFrame {
        self.call_frames[self.frame_pointer].as_ref().unwrap()
    }

    fn _top_mut(&mut self) -> &mut CallFrame {
        self.call_frames[self.frame_pointer].as_mut().unwrap()
    }
}


// impl <'src> StackFrame<'src> {
//     pub fn base(code_chunk: &'src CodeChunk) -> Self {
//         Self {
//             slots: vec![None; 8],
//             return_address: 0,
//             is_base: true,
//             upvalues: Rc::new([]),
//             code_chunk,
//         }
//     }
//
//     pub fn new(code_chunk: &'src CodeChunk, return_address: Address, upvalues: Rc<[usize]>) -> Self {
//         Self {
//             slots: vec![None; 8],
//             return_address,
//             is_base: false,
//             code_chunk,
//             upvalues,
//         }
//     }
//
//     pub fn resize_if_needed(&mut self, index: usize) {
//         let size = self.slots.len();
//         if(index > size) {
//             let new_size = f64::log2(index as f64 / size as f64).ceil() as usize;
//             self.slots.resize(new_size, None);
//         }
//     }
//
//     pub fn load(&self, index: usize) -> StackValue {
//         (**self.slots[index].as_ref().unwrap()).clone()
//     }
//
//     pub fn store(&mut self, index: usize, value: StackValue) {
//         self.resize_if_needed(index);
//         match &mut self.slots[index] {
//             None => {
//                 self.slots[index] = Some(StackSlot::new(value))
//             }
//             Some(slot) => {
//                 **slot = value;
//             }
//         }
//         // self.slots[index] = Some(value);
//     }
//
//     pub fn get_upvalue_idx(&self, index: usize) -> usize {
//         self.upvalues[index]
//     }
// }